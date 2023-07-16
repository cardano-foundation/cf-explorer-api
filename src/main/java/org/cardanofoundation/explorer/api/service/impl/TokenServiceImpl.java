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
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
import org.cardanofoundation.explorer.api.repository.TokenInfoCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.TokenInfoRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
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
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) {
    int tokenCount = aggregatedDataCacheService.getTokenCount();

    if (tokenCount == 0) {
      tokenCount = (int) multiAssetRepository.count();
    }

    var dataContent = multiAssetRepository.findMultiAssets(pageable);

    Page<MultiAsset> multiAssets = new PageImpl<>(dataContent, pageable, tokenCount);

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
    var now =  LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
    multiAssetResponsesList.forEach(ma -> {
      var tokenInfo = tokenInfoMap.getOrDefault(ma.getId(), null);

      if (tokenInfo == null) {
        ma.setNumberOfHolders(0L);
        ma.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
      } else {
        if (tokenInfo.getUpdateTime().toLocalDateTime().plusDays(1).isBefore(now)) {
          ma.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
        } else {
          ma.setVolumeIn24h(String.valueOf(tokenInfo.getVolume24h()));
        }
        ma.setNumberOfHolders(tokenInfo.getNumberOfHolders());
      }

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

    var tokenInfo = tokenInfoRepository.findTokenInfoByMultiAssetId(multiAsset.getId());
    var now =  LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    if (tokenInfo.isEmpty()) {
      tokenResponse.setNumberOfHolders(0L);
      tokenResponse.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
    } else {
      if (tokenInfo.get().getUpdateTime().toLocalDateTime().plusDays(1).isBefore(now)) {
        tokenResponse.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
      } else {
        tokenResponse.setVolumeIn24h(String.valueOf(tokenInfo.get().getVolume24h()));
      }
      tokenResponse.setNumberOfHolders(tokenInfo.get().getNumberOfHolders());
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
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId, AnalyticType type)
      throws ExecutionException, InterruptedException {

    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
        .orElseThrow(() -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND));

    List<LocalDate> dates = getListDateAnalytic(type);
    List<CompletableFuture<TokenVolumeAnalyticsResponse>> futureTokenAnalytics = new ArrayList<>();

    ExecutorService fixedExecutor = Executors.newFixedThreadPool(TOKEN_VOLUME_ANALYTIC_NUMBER);
    final Optional<LocalDate> maxDateAgg = aggregateAddressTokenRepository.getMaxDay();
    for (int i = 0; i < dates.size() - 1; i++) {
      LocalDate startRange = dates.get(i);
      LocalDate endRange = dates.get(i + 1);
      futureTokenAnalytics.add(CompletableFuture.supplyAsync(
          () -> getTokenVolumeAnalyticsResponse(multiAsset, startRange, endRange, maxDateAgg),
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
  private TokenVolumeAnalyticsResponse getTokenVolumeAnalyticsResponse(
      MultiAsset multiAsset,
      LocalDate from, LocalDate to,
      Optional<LocalDate> maxDateAgg) {

    if (maxDateAgg.isEmpty() || !from.isBefore(maxDateAgg.get())) {
      BigInteger balance = addressTokenRepository.sumBalanceBetweenTx(
          multiAsset,
          Timestamp.valueOf(from.atTime(LocalTime.MAX)),
          Timestamp.valueOf(to.atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);
      return new TokenVolumeAnalyticsResponse(to, balance);
    }

    BigInteger balance;
    if (LocalDate.now().isEqual(to)) {
      balance = getBalanceInRangeHaveToday(multiAsset, from, to, maxDateAgg.get());
    } else {
      balance = getBalanceInRangePreviousToday(multiAsset, from, to, maxDateAgg.get());
    }

    if (BigInteger.ZERO.equals(balance)) {
      Long numberBalanceRecord = addressTokenRepository.countRecord(
          multiAsset,
          Timestamp.valueOf(from.atTime(LocalTime.MAX)),
          Timestamp.valueOf(to.atTime(LocalTime.MAX)));
      boolean isNoRecord = numberBalanceRecord == null || numberBalanceRecord == 0;
      balance = isNoRecord ? null : balance;
    }

    return new TokenVolumeAnalyticsResponse(to, balance);
  }

  private BigInteger getBalanceInRangeHaveToday(MultiAsset multiAsset,
                                                LocalDate from, LocalDate to,
                                                LocalDate maxDateAgg) {
    BigInteger todayBalance = addressTokenRepository.sumBalanceBetweenTx(
        multiAsset,
        Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX))
    ).orElse(BigInteger.ZERO);

    //case missing aggregation data
    boolean isMissingAggregationData = to.minusDays(1).isAfter(maxDateAgg);
    if (isMissingAggregationData) {
      BigInteger balanceAgg = aggregateAddressTokenRepository
          .sumBalanceInTimeRange(multiAsset.getId(), from, maxDateAgg)
          .orElse(BigInteger.ZERO);

      //get missing aggregation data
      BigInteger balanceNotAgg = addressTokenRepository.sumBalanceBetweenTx(
          multiAsset,
          Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
          Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);

      return todayBalance.add(balanceAgg).add(balanceNotAgg);
    } else {
      BigInteger rangeToYesterdayBalance = aggregateAddressTokenRepository
          .sumBalanceInTimeRange(multiAsset.getId(), from, to.minusDays(1))
          .orElse(BigInteger.ZERO);
      return todayBalance.add(rangeToYesterdayBalance);
    }
  }

  private BigInteger getBalanceInRangePreviousToday(MultiAsset multiAsset,
                                                    LocalDate from, LocalDate to,
                                                    LocalDate maxDateAgg) {
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
    return "\"" + value + "\"";
  }
}