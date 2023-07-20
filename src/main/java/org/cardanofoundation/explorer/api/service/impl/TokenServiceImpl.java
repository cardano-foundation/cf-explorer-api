package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenVolumeAnalyticsResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.TokenInfoRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.TokenInfo;

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

  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AggregateAddressTokenRepository aggregateAddressTokenRepository;
  private final AggregatedDataCacheService aggregatedDataCacheService;
  private final TokenInfoRepository tokenInfoRepository;

  static final Integer TOKEN_VOLUME_ANALYTIC_NUMBER = 5;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(String query, Pageable pageable) {
    int tokenCount = aggregatedDataCacheService.getTokenCount();

    if (tokenCount == 0) {
      tokenCount = (int) multiAssetRepository.count();
    }
    List<MultiAsset> dataContent;
    Page<MultiAsset> multiAssets;
    if (StringUtils.isEmpty(query)) {
      dataContent = multiAssetRepository.findMultiAssets(pageable);
      multiAssets = new PageImpl<>(dataContent, pageable, tokenCount);
    } else {
      pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());
      multiAssets = multiAssetRepository.findAll(query.toLowerCase(), pageable);
    }

    var multiAssetResponsesList = multiAssets.map(tokenMapper::fromMultiAssetToFilterResponse);
    List<Long> multiAssetIds = StreamUtil.mapApply(multiAssets.getContent(),
        MultiAsset::getId);

    var tokenInfos = tokenInfoRepository.findTokenInfosByMultiAssetIdIn(multiAssetIds);
    var tokenInfoMap = StreamUtil.toMap(tokenInfos, TokenInfo::getMultiAssetId,
        Function.identity());

    Set<String> subjects = StreamUtil.mapApplySet(multiAssets.getContent(),
        ma -> ma.getPolicy() + ma.getName());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = StreamUtil.toMap(assetMetadataList,
        AssetMetadata::getSubject);

    multiAssetResponsesList.forEach(ma -> {
      ma.setNumberOfHolders(tokenInfoMap.containsKey(ma.getId()) ?
                            tokenInfoMap.get(ma.getId()).getNumberOfHolders() : 0);
      ma.setVolumeIn24h(String.valueOf(tokenInfoMap.containsKey(ma.getId()) ?
                                       tokenInfoMap.get(ma.getId()).getVolume24h()
                                                                            : BigInteger.ZERO));
      ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
          assetMetadataMap.get(ma.getPolicy() + ma.getName()))
      );
    });

    return new BaseFilterResponse<>(multiAssetResponsesList);
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
    Page<AddressTokenProjection> tokenAddresses
        = addressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(multiAsset, pageable);
    Set<Long> addressIds = tokenAddresses.stream().map(AddressTokenProjection::getAddressId)
        .collect(Collectors.toSet());
    List<Address> addressList = addressRepository.findAddressByIdIn(addressIds);
    Map<Long, Address> addressMap = addressList.stream().collect(
        Collectors.toMap(Address::getId, Function.identity()));
    Page<TokenAddressResponse> tokenAddressResponses = tokenAddresses.map(
        tokenMapper::fromAddressTokenProjection);
    tokenAddressResponses.forEach(tokenAddress -> {
      tokenAddress.setAddress(
              addressMap.get(tokenAddress.getAddressId()).getAddress());
      tokenAddress.setAddressId(null);
    });
    return new BaseFilterResponse<>(tokenAddressResponses);
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
      LocalDate from = fromDateTime.toLocalDate();
      LocalDate to = toDateTime.toLocalDate();
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
    return new TokenVolumeAnalyticsResponse(toDateTime, balance);
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
    if(!multiAsset.getSupply().equals(BigInteger.ONE)){
      tokenResponse.setTokenType(TokenType.FT);
    } else{
      String tsQuery = makeQuery(multiAsset.getPolicy()) + " & " + makeQuery(multiAsset.getNameView());
      String txMetadataNFTToken = maTxMintRepository.getTxMetadataNFTToken(tsQuery);
      if (txMetadataNFTToken == null || txMetadataNFTToken.isEmpty()) {
        tokenResponse.setTokenType(TokenType.FT);
      } else{
        tokenResponse.setTokenType(TokenType.NFT);
        tokenResponse.setMetadataJson(txMetadataNFTToken);
      }
    }
  }

  private String makeQuery(String value){
    return "\"" + value.replaceAll("\\s+","") + "\"";
  }
}
