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
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.common.enumeration.AddressType;
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
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.explorer.TokenInfoRepository;
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
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.TokenInfo;
import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressToken;

@Service
@RequiredArgsConstructor
@Log4j2
public class TokenServiceImpl implements TokenService {

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressTokenRepository addressTokenRepository;
  private final AddressRepository addressRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final StakeAddressRepository stakeAddressRepository;

  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AggregateAddressTokenRepository aggregateAddressTokenRepository;
  private final AggregatedDataCacheService aggregatedDataCacheService;
  private final TokenInfoRepository tokenInfoRepository;
  private static final int MAX_TOTAL_ELEMENTS = 1000;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(String query, Pageable pageable) {
    int tokenCount = aggregatedDataCacheService.getTokenCount();

    if (tokenCount == 0) {
      tokenCount = (int) multiAssetRepository.count();
    }
    List<MultiAsset> dataContent;
    Page<MultiAsset> multiAssets;
    boolean isQueryEmpty = StringUtils.isEmpty(query);
    if(isQueryEmpty){
      pageable = createPageableWithSort(pageable, Sort.by(Sort.Direction.DESC, MultiAsset_.TX_COUNT, MultiAsset_.SUPPLY));
      dataContent = multiAssetRepository.findMultiAssets(pageable);
      multiAssets = new PageImpl<>(dataContent, pageable, tokenCount);
    } else {
      String lengthOfNameView = "nameViewLength";
      if(MAX_TOTAL_ELEMENTS / pageable.getPageSize() <= pageable.getPageNumber()){
        throw new BusinessException(BusinessCode.OUT_OF_QUERY_LIMIT);
      }
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
  public BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable) {
    Page<MaTxMint> maTxMints = maTxMintRepository.findByIdent(tokenId, pageable);
    return new BaseFilterResponse<>(maTxMints.map(maTxMintMapper::fromMaTxMintToTokenMintTx));
  }

  @Override
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
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId, AnalyticType type) {

    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId)
        .orElseThrow(() -> new NoContentException(BusinessCode.TOKEN_NOT_FOUND));

    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);

    List<TokenVolumeAnalyticsResponse> responses = new ArrayList<>();
    if (AnalyticType.ONE_DAY.equals(type)) {
      for (int i = 0; i < dates.size() - 1; i++) {
        BigInteger balance = addressTokenRepository.sumBalanceBetweenTx(
            multiAsset, Timestamp.valueOf(dates.get(i)), Timestamp.valueOf(dates.get(i + 1))
        ).orElse(BigInteger.ZERO);
        responses.add(new TokenVolumeAnalyticsResponse(dates.get(i), balance));
      }
    } else {
      dates.remove(dates.size() - 1);
      List<AggregateAddressToken> aggregateAddressTokens = aggregateAddressTokenRepository
          .findAllByIdentAndDayBetween(
              multiAsset.getId(),
              dates.get(0).toLocalDate(),
              dates.get(dates.size() - 1).toLocalDate());
      Map<LocalDate, BigInteger> aggregateAddressTokenMap = StreamUtil.toMap(
          aggregateAddressTokens,
          AggregateAddressToken::getDay,
          AggregateAddressToken::getBalance
      );
      for (LocalDateTime date : dates) {
        TokenVolumeAnalyticsResponse tokenVolume = new TokenVolumeAnalyticsResponse(
            date, aggregateAddressTokenMap.getOrDefault(date.toLocalDate(), BigInteger.ZERO));
        responses.add(tokenVolume);
      }
    }
    return responses;
  }

  private void setTxMetadataJson(TokenResponse tokenResponse, MultiAsset multiAsset) {
    if (multiAsset.getSupply().compareTo(BigInteger.ONE) == 0) {
      tokenResponse.setTokenType(TokenType.NFT);
    } else {
      tokenResponse.setTokenType(TokenType.FT);
    }
    tokenResponse.setMetadataJson(
        maTxMintRepository.getTxMetadataToken(multiAsset.getFingerprint(), CommonConstant.METADATA_LABEL_721));
  }
}
