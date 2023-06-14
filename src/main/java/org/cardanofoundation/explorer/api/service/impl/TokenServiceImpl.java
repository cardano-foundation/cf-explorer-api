package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
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
import org.cardanofoundation.explorer.api.projection.TokenNumberHoldersProjection;
import org.cardanofoundation.explorer.api.projection.TokenVolumeProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.repository.aggregation.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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

  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AggregateAddressTokenRepository aggregateAddressTokenRepository;

  @Qualifier("taskExecutor")
  private final TaskExecutor taskExecutor;

  static final Integer TOKEN_VOLUME_ANALYTIC_NUMBER = 5;


  @SingletonCall(typeToken = TypeTokenGson.TOKEN_FILTER, expireAfterSeconds = 200)
  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable)
      throws ExecutionException, InterruptedException {
    Page<MultiAsset> multiAssets = multiAssetRepository.findAll(pageable);
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
    return new BaseFilterResponse<>(multiAssetResponsesList);
  }


  @Override
  @SingletonCall(typeToken = TypeTokenGson.TOKEN_DETAIL, expireAfterSeconds = 100)
  @Transactional(readOnly = true)
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
            .orElseThrow(() -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND));

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
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId, AnalyticType type)
      throws ExecutionException, InterruptedException {

    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
        .orElseThrow(() -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND));

    List<LocalDate> dates = getListDateAnalytic(type);
    List<CompletableFuture<TokenVolumeAnalyticsResponse>> futureTokenAnalytics = new ArrayList<>();

    ExecutorService fixedExecutor = Executors.newFixedThreadPool(TOKEN_VOLUME_ANALYTIC_NUMBER);
    for (int i = 0; i < dates.size() - 1; i++) {
      LocalDate startRange = dates.get(i);
      LocalDate endRange = dates.get(i + 1);
      futureTokenAnalytics.add(CompletableFuture.supplyAsync(
          () -> getTokenVolumeAnalyticsResponse(multiAsset, startRange, endRange),
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

  /**
   * if param `to` less than today: get data from table: `agg_address_token`
   * else: get data from table `address_token` for today
   * and get data in range: `from` - previous day of `to` from table: `agg_address_token`
   * and then sum data of 2 queries
   * **/
  private TokenVolumeAnalyticsResponse getTokenVolumeAnalyticsResponse(MultiAsset multiAsset,
                                                                       LocalDate from, LocalDate to) {
    BigInteger balance = BigInteger.ZERO;
    if (LocalDate.now().isEqual(to)) {
      BigInteger todayBalance = addressTokenRepository.sumBalanceBetweenTx(
              multiAsset,
              Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);

      BigInteger rangeToYesterdayBalance = aggregateAddressTokenRepository
              .sumBalanceInTimeRange(multiAsset.getId(), from, to.minusDays(1))
              .orElse(BigInteger.ZERO);

      balance = balance.add(todayBalance).add(rangeToYesterdayBalance);
    } else {
      balance = aggregateAddressTokenRepository
              .sumBalanceInTimeRange(multiAsset.getId(), from, to).orElse(BigInteger.ZERO);
    }

    return new TokenVolumeAnalyticsResponse(to, balance);
  }

  private List<LocalDate> getListDateAnalytic(AnalyticType analyticType) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    List<LocalDate> dates = new ArrayList<>();
    switch (analyticType) {
      case ONE_WEEK -> {
        var currentWeek = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentWeek.minusWeeks(i));
        }
      }
      case ONE_MONTH -> {
        var currentMonth = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentMonth.minusMonths(i));
        }
      }
      case THREE_MONTH -> {
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
      }
      default -> {
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentDate.minusDays(i));
        }
      }
    }
    return dates;
  }
}
