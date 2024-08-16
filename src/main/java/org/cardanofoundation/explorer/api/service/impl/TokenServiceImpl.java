package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
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
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.api.repository.explorer.TokenInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.LatestTokenBalanceRepository;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.api.util.MetadataCIP25Utils;
import org.cardanofoundation.explorer.api.util.MetadataCIP60Utils;
import org.cardanofoundation.explorer.api.util.StreamUtil;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.explorer.TokenInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.AssetMetadata;
import org.cardanofoundation.explorer.common.entity.ledgersync.MaTxMint;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset_;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.entity.ledgersync.TokenTxCount_;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AggregateAddressToken;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class TokenServiceImpl implements TokenService {

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final LatestTokenBalanceRepository latestTokenBalanceRepository;
  private final AddressTxAmountRepository addressTxAmountRepository;
  private final ScriptRepository scriptRepository;

  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AggregateAddressTokenRepository aggregateAddressTokenRepository;
  private final AggregatedDataCacheService aggregatedDataCacheService;
  private final TokenInfoRepository tokenInfoRepository;
  private final CardanoConverters cardanoConverters;
  private static final int MAX_TOTAL_ELEMENTS = 1000;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(String query, Pageable pageable) {
    int tokenCount = aggregatedDataCacheService.getTokenCount();

    if (tokenCount == 0) {
      tokenCount = (int) multiAssetRepository.count();
    }
    Page<TokenProjection> multiAssets;
    boolean isQueryEmpty = StringUtils.isEmpty(query);
    if (isQueryEmpty) {
      pageable =
          createPageableWithSort(
              pageable, Sort.by(Sort.Direction.DESC, TokenTxCount_.TX_COUNT, MultiAsset_.SUPPLY));
      multiAssets =
          new PageImpl<>(multiAssetRepository.findMultiAssets(pageable), pageable, tokenCount);
    } else {
      final String lengthOfNameView = "nameViewLength";
      if (MAX_TOTAL_ELEMENTS / pageable.getPageSize() <= pageable.getPageNumber()) {
        throw new BusinessException(BusinessCode.OUT_OF_QUERY_LIMIT);
      }
      pageable =
          createPageableWithSort(
              pageable,
              Sort.by(Sort.Direction.ASC, lengthOfNameView, MultiAsset_.NAME_VIEW)
                  .and(Sort.by(Sort.Direction.DESC, TokenTxCount_.TX_COUNT)));
      Long count = multiAssetRepository.countAllByQuery(query.toLowerCase());
      multiAssets =
          new PageImpl<>(
              multiAssetRepository.findAll(query.toLowerCase(), pageable), pageable, count);
    }
    Page<TokenFilterResponse> multiAssetResponsesList =
        multiAssets.map(assetMetadataMapper::fromTokenProjectionToTokenFilterResponse);
    List<String> multiAssetIds =
        StreamUtil.mapApply(multiAssets.getContent(), TokenProjection::getUnit);
    List<TokenInfo> tokenInfos = tokenInfoRepository.findTokenInfosByUnitIn(multiAssetIds);
    Map<String, TokenInfo> tokenInfoMap =
        StreamUtil.toMap(tokenInfos, TokenInfo::getUnit, Function.identity());

    Map<String, Script> scriptMap =
        scriptRepository
            .findAllByHashIn(
                multiAssets.getContent().stream()
                    .map(TokenProjection::getPolicy)
                    .collect(Collectors.toList()))
            .stream()
            .collect(Collectors.toMap(Script::getHash, Function.identity()));

    LocalDateTime now = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);
    multiAssetResponsesList.forEach(
        ma -> {
          TokenInfo tokenInfo = tokenInfoMap.getOrDefault(ma.getUnit(), null);
          Script script = scriptMap.get(ma.getPolicy());
          if (Objects.nonNull(script)) {
            ma.setPolicyIsNativeScript(ScriptType.TIMELOCK.equals(script.getType()));
          } else {
            ma.setPolicyIsNativeScript(true);
          }

          if (tokenInfo == null) {
            ma.setNumberOfHolders(0L);
            ma.setTotalVolume(String.valueOf(BigInteger.ZERO));
            ma.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
          } else {
            LocalDateTime updatedTime =
                cardanoConverters.slot().slotToTime(tokenInfo.getUpdatedSlot());
            if (updatedTime.plusDays(1).isBefore(now)) {
              ma.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
            } else {
              ma.setVolumeIn24h(String.valueOf(tokenInfo.getVolume24h()));
            }
            ma.setTotalVolume(String.valueOf(tokenInfo.getTotalVolume()));
            ma.setNumberOfHolders(tokenInfo.getNumberOfHolders());
          }
        });

    if (isQueryEmpty) {
      return new BaseFilterResponse<>(multiAssetResponsesList);
    } else {
      return new BaseFilterResponse<>(
          multiAssetResponsesList, multiAssets.getTotalElements() >= 1000);
    }
  }

  /**
   * Create pageable with sort, if sort is unsorted then use default sort
   *
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
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset =
        multiAssetRepository
            .findByFingerprint(tokenId)
            .orElseThrow(() -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));

    TokenResponse tokenResponse = tokenMapper.fromMultiAssetToResponse(multiAsset);
    Long tokenTxCount =
        multiAssetRepository.getTokenTxCount(multiAsset.getFingerprint()).orElse(0L);
    tokenResponse.setTxCount(tokenTxCount.intValue());
    var tokenInfo = tokenInfoRepository.findTokenInfoByUnit(multiAsset.getUnit());
    var now = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC);

    if (tokenInfo.isEmpty()) {
      tokenResponse.setNumberOfHolders(0L);
      tokenResponse.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
      tokenResponse.setTotalVolume(String.valueOf(BigInteger.ZERO));
    } else {
      LocalDateTime updatedTime =
          cardanoConverters.slot().slotToTime(tokenInfo.get().getUpdatedSlot());
      if (updatedTime.plusDays(1).isBefore(now)) {
        tokenResponse.setVolumeIn24h(String.valueOf(BigInteger.ZERO));
      } else {
        tokenResponse.setVolumeIn24h(String.valueOf(tokenInfo.get().getVolume24h()));
      }
      tokenResponse.setTotalVolume(String.valueOf(tokenInfo.get().getTotalVolume()));
      tokenResponse.setNumberOfHolders(tokenInfo.get().getNumberOfHolders());
    }

    AssetMetadata assetMetadata =
        assetMetadataRepository
            .findFirstBySubject(multiAsset.getPolicy() + multiAsset.getName())
            .orElse(null);

    tokenResponse.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadata));

    Long latestEpochTime =
        addressTxAmountRepository.getLastActivityTimeOfToken(multiAsset.getUnit());
    tokenResponse.setTokenLastActivity(
        Timestamp.valueOf(
            LocalDateTime.ofInstant(Instant.ofEpochSecond(latestEpochTime), ZoneOffset.UTC)));
    setTxMetadataJson(tokenResponse, multiAsset);
    return tokenResponse;
  }

  @Override
  public BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable) {
    Page<MaTxMint> maTxMints = maTxMintRepository.findByIdent(tokenId, pageable);
    return new BaseFilterResponse<>(maTxMints.map(maTxMintMapper::fromMaTxMintToTokenMintTx));
  }

  @Override
  public BaseFilterResponse<TokenAddressResponse> getTopHolders(String tokenId, Pageable pageable) {
    MultiAsset multiAsset =
        multiAssetRepository
            .findByFingerprint(tokenId)
            .orElseThrow(() -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));

    long numberOfHolders =
        tokenInfoRepository
            .findTokenInfoByUnit(multiAsset.getUnit())
            .map(TokenInfo::getNumberOfHolders)
            .orElse(0L);

    List<TokenAddressResponse> tokenAddressResponses =
        latestTokenBalanceRepository.getTopHolderOfToken(multiAsset.getUnit(), pageable).stream()
            .map(tokenMapper::fromAddressTokenProjection)
            .collect(Collectors.toList());

    Page<TokenAddressResponse> tokenAddressResponsePage =
        new PageImpl<>(tokenAddressResponses, pageable, numberOfHolders);

    return new BaseFilterResponse<>(tokenAddressResponsePage);
  }

  @Override
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(
      String tokenId, AnalyticType type) {

    MultiAsset multiAsset =
        multiAssetRepository
            .findByFingerprint(tokenId)
            .orElseThrow(() -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));

    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);

    List<TokenVolumeAnalyticsResponse> responses = new ArrayList<>();
    if (AnalyticType.ONE_DAY.equals(type)) {
      for (int i = 0; i < dates.size() - 1; i++) {
        BigInteger balance =
            addressTxAmountRepository
                .sumBalanceByUnitAndSlotBetween(
                    multiAsset.getUnit(),
                    cardanoConverters.time().toSlot(dates.get(i)),
                    cardanoConverters.time().toSlot(dates.get(i + 1)))
                .orElse(BigInteger.ZERO);
        responses.add(new TokenVolumeAnalyticsResponse(dates.get(i), balance));
      }
    } else {
      dates.remove(dates.size() - 1);
      List<AggregateAddressToken> aggregateAddressTokens =
          aggregateAddressTokenRepository.findAllByUnitAndDayBetween(
              multiAsset.getUnit(),
              dates.get(0).toLocalDate(),
              dates.get(dates.size() - 1).toLocalDate());
      Map<LocalDate, BigInteger> aggregateAddressTokenMap =
          StreamUtil.toMap(
              aggregateAddressTokens,
              AggregateAddressToken::getDay,
              AggregateAddressToken::getBalance);
      for (LocalDateTime date : dates) {
        TokenVolumeAnalyticsResponse tokenVolume =
            new TokenVolumeAnalyticsResponse(
                date, aggregateAddressTokenMap.getOrDefault(date.toLocalDate(), BigInteger.ZERO));
        responses.add(tokenVolume);
      }
    }
    return responses;
  }

  private void setTxMetadataJson(TokenResponse tokenResponse, MultiAsset multiAsset) {
    if (multiAsset.getSupply().equals(BigInteger.ONE)
        || (multiAsset.getSupply().equals(BigInteger.ZERO)
            && maTxMintRepository.mintNumber(multiAsset.getFingerprint()))) {
      tokenResponse.setTokenType(TokenType.NFT);
    } else {
      tokenResponse.setTokenType(TokenType.FT);
    }
    String assetName =
        Objects.isNull(multiAsset.getNameView()) ? multiAsset.getName() : multiAsset.getNameView();
    tokenResponse.setMetadataJson(
        MetadataCIP25Utils.splitJsonMetadataByAssetName(
            maTxMintRepository.getTxMetadataToken(
                multiAsset.getFingerprint(), CommonConstant.METADATA_LABEL_721),
            assetName));
    tokenResponse.setMetadataCIP25(MetadataCIP25Utils.standard(tokenResponse.getMetadataJson()));
    tokenResponse.setMetadataCIP60(MetadataCIP60Utils.standard(tokenResponse.getMetadataJson()));
  }
}
