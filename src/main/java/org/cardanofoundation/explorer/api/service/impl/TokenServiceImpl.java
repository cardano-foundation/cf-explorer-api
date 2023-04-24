package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.service.TokenService;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.util.List;
import java.util.Map;
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
  private final AddressRepository addressRepository;
  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;
  private final AssetMetadataMapper assetMetadataMapper;


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
    multiAssetResponsesList.forEach(
        ma -> ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
            assetMetadataMap.get(ma.getPolicy() + ma.getName()))
        )
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
}
