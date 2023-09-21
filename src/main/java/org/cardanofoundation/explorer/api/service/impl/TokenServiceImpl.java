package org.cardanofoundation.explorer.api.service.impl;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.common.enumeration.AddressType;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.cache.TokenPageCacheService;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.*;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.api.projection.TokenVolumeProjection;
import org.cardanofoundation.explorer.api.projection.TokenNumberHoldersProjection;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class TokenServiceImpl implements TokenService {

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressTokenRepository addressTokenRepository;
  private final AddressRepository addressRepository;
  private final TxRepository txRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final StakeAddressRepository stakeAddressRepository;

  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AggregateAddressTokenRepository aggregateAddressTokenRepository;
  private final TokenPageCacheService tokenPageCacheService;
  @Qualifier("taskExecutor")
  private final TaskExecutor taskExecutor;

//  @SingletonCall(typeToken = TypeTokenGson.TOKEN_FILTER, expireAfterSeconds = 150, callAfterMilis = 200)
  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(String query, Pageable pageable)
      throws ExecutionException, InterruptedException {

    Page<MultiAsset> multiAssets;
    boolean isQueryEmpty = StringUtils.isEmpty(query);
    if(isQueryEmpty){
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.DESC, MultiAsset_.TX_COUNT, MultiAsset_.SUPPLY));
      Optional<BaseFilterResponse<TokenFilterResponse>> cacheResp =
          tokenPageCacheService.getTokePageCache(pageable);
      if (cacheResp.isPresent()) {
        return cacheResp.get();
      } else {
        multiAssets = multiAssetRepository.findAll(pageable);
      }
    } else {
      String lengthOfNameView = "nameViewLength";
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.ASC, lengthOfNameView, MultiAsset_.NAME_VIEW)
          .and(Sort.by(Sort.Direction.DESC, MultiAsset_.TX_COUNT)));
      multiAssets = multiAssetRepository.findAll(query.toLowerCase(), pageable).map(
          item -> {
            MultiAsset multiAsset = new MultiAsset();
            multiAsset.setId(item.getId());
            multiAsset.setPolicy(item.getPolicy());
            multiAsset.setName(item.getName());
            multiAsset.setNameView(item.getNameView());
            multiAsset.setFingerprint(item.getFingerprint());
            multiAsset.setTxCount(item.getTxCount());
            multiAsset.setSupply(item.getSupply());
            multiAsset.setTotalVolume(item.getTotalVolume());
            multiAsset.setTime(item.getTime());
            return multiAsset;
          }
      );
    }
    Set<String> subjects = StreamUtil.mapApplySet(multiAssets.getContent(), ma -> ma.getPolicy() + ma.getName());

    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = StreamUtil.toMap(assetMetadataList, AssetMetadata::getSubject);

    var multiAssetResponsesList = multiAssets.map(tokenMapper::fromMultiAssetToFilterResponse);
    Timestamp yesterday = Timestamp.valueOf(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).minusDays(1));
    Long txId = txRepository.findMinTxByAfterTime(yesterday).orElse(Long.MAX_VALUE);
    List<TokenVolumeProjection> volumes = addressTokenRepository.sumBalanceAfterTx(multiAssets.getContent(), txId);

    List<Long> multiAssetIds = StreamUtil.mapApply(multiAssets.getContent(), MultiAsset::getId);
    var numberOfHoldersWithStakeKeyAsync = CompletableFuture.supplyAsync(
        () -> addressTokenBalanceRepository.countByMultiAssetIn(multiAssetIds), taskExecutor);

    var numberOfHoldersWithAddressNotHaveStakeKeyAsync = CompletableFuture.supplyAsync(
        () -> addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAssetIn(multiAssetIds),
        taskExecutor);

    CompletableFuture.allOf(numberOfHoldersWithStakeKeyAsync, numberOfHoldersWithAddressNotHaveStakeKeyAsync).join();
    Map<Long, Long> numberHoldersStakeKeyMap = StreamUtil.toMap(
        numberOfHoldersWithStakeKeyAsync.get(),
        TokenNumberHoldersProjection::getIdent,
        TokenNumberHoldersProjection::getNumberOfHolders
    );

    Map<Long, Long> numberHoldersAddressNotHaveStakeKeyMap = StreamUtil.toMap(
        numberOfHoldersWithAddressNotHaveStakeKeyAsync.get(),
        TokenNumberHoldersProjection::getIdent,
        TokenNumberHoldersProjection::getNumberOfHolders
    );

    Map<Long, BigInteger> tokenVolumeMap = StreamUtil
        .toMap(volumes, TokenVolumeProjection::getIdent, TokenVolumeProjection::getVolume);

    multiAssetResponsesList.forEach(
        ma -> {
          ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
              assetMetadataMap.get(ma.getPolicy() + ma.getName()))
          );
          if (tokenVolumeMap.containsKey(ma.getId())) {
            ma.setVolumeIn24h(tokenVolumeMap.get(ma.getId()).toString());
          } else {
            ma.setVolumeIn24h(String.valueOf(0));
          }
          ma.setVolumeIn24h(tokenVolumeMap.getOrDefault(ma.getId(), BigInteger.valueOf(0)).toString());
          ma.setNumberOfHolders(numberHoldersStakeKeyMap.getOrDefault(ma.getId(), 0L)
              + numberHoldersAddressNotHaveStakeKeyMap.getOrDefault(ma.getId(), 0L));
          ma.setId(null);
        }
    );
    if(isQueryEmpty){
      return new BaseFilterResponse<>(multiAssetResponsesList);
    } else {
      return new BaseFilterResponse<>(multiAssetResponsesList, multiAssets.getTotalElements() >= 1000);
    }
  }

  /**
   * Create pageable with sort, if sort is unsorted then use default sort
   * @param pageable page information
   * @param defaultSort default sort condition
   * @return pageable with sort
   */
  private Pageable createPageableWithSort(Pageable pageable, Sort defaultSort) {
    Sort sort = pageable.getSort();
    if (sort.isUnsorted()) {
      sort = defaultSort;
    }
    pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), sort);
    return pageable;
  }

  @Override
  @Transactional(readOnly = true)
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
        .orElseThrow(() -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));

    TokenResponse tokenResponse = tokenMapper.fromMultiAssetToResponse(multiAsset);
    Timestamp yesterday = Timestamp.valueOf(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).minusDays(1));
    Long txId = txRepository.findMinTxByAfterTime(yesterday).orElse(Long.MAX_VALUE);
    var volume = addressTokenRepository.sumBalanceAfterTx(multiAsset, txId);

    var numberOfHoldersHaveStakeKey = addressTokenBalanceRepository
        .countAddressNotHaveStakeByMultiAsset(multiAsset)
        .orElse(0L);
    var numberOfHoldersNotHaveStakeKe =  addressTokenBalanceRepository
        .countStakeByMultiAsset(multiAsset)
        .orElse(0L);

    tokenResponse.setNumberOfHolders(numberOfHoldersHaveStakeKey + numberOfHoldersNotHaveStakeKe);
    if (Objects.isNull(volume)) {
      tokenResponse.setVolumeIn24h(String.valueOf(0));
    } else {
      tokenResponse.setVolumeIn24h(volume.toString());
    }
    AssetMetadata assetMetadata = assetMetadataRepository
        .findFirstBySubject(multiAsset.getPolicy() + multiAsset.getName())
        .orElse(null);

    tokenResponse.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadata));
    tokenResponse.setTokenLastActivity(multiAssetRepository.getLastActivityTimeOfToken(multiAsset));
    setTxMetadataJson(tokenResponse, multiAsset);
    return tokenResponse;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable) {
    Page<MaTxMint> maTxMints = maTxMintRepository.findByIdent(tokenId, pageable);
    return new BaseFilterResponse<>(maTxMints.map(maTxMintMapper::fromMaTxMintToTokenMintTx));
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTopHolders(String tokenId, Pageable pageable) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId).orElseThrow(
        () -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND)
    );
    List<AddressTokenProjection> tokenAddresses
        = addressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(multiAsset, pageable);

    var numberOfHoldersHaveStakeKey = addressTokenBalanceRepository
        .countAddressNotHaveStakeByMultiAsset(multiAsset)
        .orElse(0L);
    var numberOfHoldersNotHaveStakeKe =  addressTokenBalanceRepository
        .countStakeByMultiAsset(multiAsset)
        .orElse(0L);
    var numberOfHolder = numberOfHoldersHaveStakeKey + numberOfHoldersNotHaveStakeKe;
    Set<Long> stakeAddressIds = tokenAddresses.stream()
        .map(AddressTokenProjection::getAddressId)
        .filter(addressId -> addressId > 0L)
        .collect(Collectors.toSet());
    List<StakeAddress> stakeAddressList = stakeAddressRepository.findByIdIn(stakeAddressIds);
    Map<Long, StakeAddress> stakeAddressMap = stakeAddressList.stream().collect(
        Collectors.toMap(StakeAddress::getId, Function.identity()));
    Set<Long> addressIds = tokenAddresses.stream()
        .filter(item -> item.getAddressId() < 0L)
        .map(item -> item.getAddressId() * -1L)
        .collect(Collectors.toSet());
    List<Address> addressList = addressRepository.findAddressByIdIn(addressIds);
    Map<Long, Address> addressMap = addressList.stream().collect(
        Collectors.toMap(Address::getId, Function.identity()));
    List<TokenAddressResponse> tokenAddressResponses = tokenAddresses.stream().map(
        tokenMapper::fromAddressTokenProjection).toList();
    tokenAddressResponses.forEach(tokenAddress -> {
      if (tokenAddress.getAddressId() < 0L) {
        tokenAddress.setAddress(addressMap.get(tokenAddress.getAddressId() * -1L).getAddress());
        tokenAddress.setAddressType(AddressType.PAYMENT_ADDRESS);
      } else {
        tokenAddress.setAddress(stakeAddressMap.get(tokenAddress.getAddressId()).getView());
        tokenAddress.setAddressType(AddressType.STAKE_ADDRESS);
      }
      tokenAddress.setAddressId(null);
    });
    Page<TokenAddressResponse> response = new PageImpl<>(tokenAddressResponses, pageable, numberOfHolder);
    return new BaseFilterResponse<>(response);
  }

  @Override
  @Transactional(readOnly = true)
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(
      String tokenId, AnalyticType type) throws ExecutionException, InterruptedException {

    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
        .orElseThrow(() -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND));

    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);
    List<CompletableFuture<TokenVolumeAnalyticsResponse>> futureTokenAnalytics = new ArrayList<>();
    final int NUMBER_PARALLEL = 5;
    ExecutorService fixedExecutor = Executors.newFixedThreadPool(NUMBER_PARALLEL);

    final Optional<LocalDate> maxDateAgg = aggregateAddressTokenRepository.getMaxDay();
    for (int i = 0; i < dates.size() - 1; i++) {
      LocalDateTime startRange = dates.get(i);
      LocalDateTime endRange = dates.get(i + 1);
      futureTokenAnalytics.add(CompletableFuture.supplyAsync(
          () -> getTokenVolumeAnalyticsResponse(multiAsset, type, startRange, endRange, maxDateAgg),
          fixedExecutor)
      );
    }
    CompletableFuture.allOf(futureTokenAnalytics.toArray(new CompletableFuture[0])).join();
    List<TokenVolumeAnalyticsResponse> responses = new ArrayList<>();

    for (CompletableFuture<TokenVolumeAnalyticsResponse> fRes : futureTokenAnalytics) {
      responses.add(fRes.get());
    }
    return responses;
  }

  private TokenVolumeAnalyticsResponse getTokenVolumeAnalyticsResponse(
      MultiAsset multiAsset, AnalyticType type,
      LocalDateTime fromDateTime, LocalDateTime toDateTime,
      Optional<LocalDate> maxDateAgg) {

    BigInteger balance;
    if (type == AnalyticType.ONE_DAY) {
      balance = addressTokenRepository.sumBalanceBetweenTx(
          multiAsset, Timestamp.valueOf(fromDateTime), Timestamp.valueOf(toDateTime)
      ).orElse(BigInteger.ZERO);

    } else {
      // aggregate address token is total volume of token from 00:00:00 to 23:59:59
      // so to get volume of token between from date and to date, we need to sum between from date - 1 and to date - 1
      LocalDate from = fromDateTime.toLocalDate().minusDays(1);
      LocalDate to = toDateTime.toLocalDate().minusDays(1);
      boolean isNotHaveAggData = maxDateAgg.isEmpty() || !from.isBefore(maxDateAgg.get());
      if (isNotHaveAggData) {
        balance = addressTokenRepository.sumBalanceBetweenTx(
            multiAsset,
            Timestamp.valueOf(from.atTime(LocalTime.MAX)),
            Timestamp.valueOf(to.atTime(LocalTime.MAX))
        ).orElse(BigInteger.ZERO);
      } else {
        balance = getBalance(multiAsset, from, to, maxDateAgg.get());
      }
    }
    if (BigInteger.ZERO.equals(balance)) {
      balance = checkNoRecord(multiAsset, type, fromDateTime, toDateTime) ? null : balance;
    }
    return new TokenVolumeAnalyticsResponse(fromDateTime, balance);
  }

  private boolean checkNoRecord(
      MultiAsset multiAsset, AnalyticType type,
      LocalDateTime fromDateTime, LocalDateTime toDateTime
  ) {
    Timestamp startRange;
    Timestamp endRange;
    if (type == AnalyticType.ONE_DAY) {
      startRange = Timestamp.valueOf(fromDateTime);
      endRange = Timestamp.valueOf(toDateTime);
    } else {
      startRange = Timestamp.valueOf(fromDateTime.toLocalDate().atTime(LocalTime.MAX));
      endRange = Timestamp.valueOf(toDateTime.toLocalDate().atTime(LocalTime.MAX));
    }
    Long numberBalanceRecord = addressTokenRepository.countRecord(multiAsset, startRange, endRange);
    return numberBalanceRecord == null || numberBalanceRecord == 0;
  }

  private BigInteger getBalance(MultiAsset multiAsset,
      LocalDate from, LocalDate to, LocalDate maxDateAgg) {
    boolean isNotMissingAggregationData = !to.isAfter(maxDateAgg);
    if (isNotMissingAggregationData) {
      return aggregateAddressTokenRepository
          .sumBalanceInTimeRange(multiAsset.getId(), from, to).orElse(BigInteger.ZERO);
    }

    BigInteger balanceAgg = aggregateAddressTokenRepository
        .sumBalanceInTimeRange(multiAsset.getId(), from, maxDateAgg)
        .orElse(BigInteger.ZERO);

    BigInteger balanceNotAgg = addressTokenRepository.sumBalanceBetweenTx(
        multiAsset,
        Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
        Timestamp.valueOf(to.atTime(LocalTime.MAX))
    ).orElse(BigInteger.ZERO);
    return balanceAgg.add(balanceNotAgg);
  }

  private void setTxMetadataJson(TokenResponse tokenResponse, MultiAsset multiAsset) {
    String txMetadataNFTToken = maTxMintRepository.getTxMetadataNFTToken(multiAsset.getFingerprint());
    if (txMetadataNFTToken == null || txMetadataNFTToken.isEmpty()) {
      tokenResponse.setTokenType(TokenType.FT);
    } else {
      tokenResponse.setTokenType(TokenType.NFT);
      tokenResponse.setMetadataJson(txMetadataNFTToken);
    }
  }
}
