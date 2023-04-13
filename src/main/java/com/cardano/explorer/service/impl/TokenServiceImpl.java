package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.MaTxMintMapper;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.model.response.token.TokenVolumeAnalyticsResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.projection.TokenVolumeProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.AddressTokenRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.TokenService;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
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
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
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
  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;

  static final Integer TOKEN_VOLUME_ANALYTIC_NUMBER = 5;


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) {
    Page<MultiAsset> multiAssets = multiAssetRepository.findAll(pageable);
    Set<String> subjects = multiAssets.stream().map(
        ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    var multiAssetResponsesList = multiAssets.map(tokenMapper::fromMultiAssetToFilterResponse);
    Timestamp yesterday = Timestamp.valueOf(
        LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).minusDays(1));
    List<TokenVolumeProjection> volumes = addressTokenRepository.sumBalanceAfterTx(
        multiAssets.getContent(), yesterday);
    Map<Long, BigInteger> tokenVolumeMap = volumes.stream().collect(
        Collectors.toMap(TokenVolumeProjection::getIdent, TokenVolumeProjection::getVolume));
    multiAssetResponsesList.forEach(
        ma -> {
          ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
              assetMetadataMap.get(ma.getPolicy() + ma.getName()))
          );
          if(tokenVolumeMap.containsKey(ma.getId())) {
            ma.setVolumeIn24h(tokenVolumeMap.get(ma.getId()).toString());
          } else {
            ma.setVolumeIn24h(String.valueOf(0));
          }
          ma.setId(null);
        }
    );
    return new BaseFilterResponse<>(multiAssetResponsesList);
  }


  @Override
  @Transactional(readOnly = true)
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId).orElseThrow(
        () -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND)
    );
    TokenResponse tokenResponse = tokenMapper.fromMultiAssetToResponse(multiAsset);
    Timestamp yesterday = Timestamp.valueOf(
        LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).minusDays(1));
    var volume = addressTokenRepository.sumBalanceAfterTx(multiAsset, yesterday);
    if(Objects.isNull(volume)) {
      tokenResponse.setVolumeIn24h(String.valueOf(0));
    } else {
      tokenResponse.setVolumeIn24h(volume.toString());
    }
    AssetMetadata assetMetadata = assetMetadataRepository.findFirstBySubject(
        multiAsset.getPolicy() + multiAsset.getName()).orElse(null);
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
        () -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND)
    );
    Page<AddressTokenProjection> tokenAddresses = multiAssetRepository.findAddressByToken(
        multiAsset, pageable);
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
  public List<TokenVolumeAnalyticsResponse> getTokenVolumeAnalytic(String tokenId,
      AnalyticType type) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId).orElseThrow(
        () -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND));
    List<TokenVolumeAnalyticsResponse> responses = new ArrayList<>();
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    List<LocalDate> dates = new ArrayList<>();
    switch (type) {
      case ONE_WEEK:
        var currentWeek = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentWeek.minusWeeks(i));
        }
        break;
      case ONE_MONTH:
        var currentMonth = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentMonth.minusMonths(i));
        }
        break;
      case THREE_MONTH:
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
        break;
      default:
        for (int i = TOKEN_VOLUME_ANALYTIC_NUMBER; i >= 0; i--) {
          dates.add(currentDate.minusDays(i));
        }
    }
    for (int i = 0; i < dates.size() - 1; i++) {
      TokenVolumeAnalyticsResponse response = new TokenVolumeAnalyticsResponse();
      var balance = addressTokenRepository.sumBalanceBetweenTx(multiAsset,
          Timestamp.valueOf(dates.get(i).atTime(LocalTime.MAX)),
          Timestamp.valueOf(dates.get(i + 1).atTime(LocalTime.MAX)));
      if (Objects.isNull(balance)) {
        response.setValue(BigInteger.ZERO);
      } else {
        response.setValue(balance);
      }
      response.setDate(dates.get(i+1));
      responses.add(response);
    }
    return responses;
  }
}
