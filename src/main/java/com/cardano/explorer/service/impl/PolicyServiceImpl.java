package com.cardano.explorer.service.impl;

import com.bloxbean.cardano.client.util.HexUtil;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.PolicyResponse;
import com.cardano.explorer.model.response.token.PolicyScriptResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.ScriptRepository;
import com.cardano.explorer.service.PolicyService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PolicyServiceImpl implements PolicyService {

  private final MultiAssetRepository multiAssetRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressRepository addressRepository;
  private final AssetMetadataMapper assetMetadataMapper;
  private final TokenMapper tokenMapper;
  private final ScriptRepository scriptRepository;
  private final ObjectMapper objectMapper;

  @Override
  public PolicyResponse getPolicyDetail(String policyId) {
    Integer tokenCount = multiAssetRepository.countByPolicy(policyId);
    if (Integer.valueOf(0).equals(tokenCount)) {
      throw new BusinessException(BusinessCode.POLICY_NOT_FOUND);
    }
    var policyResponse = PolicyResponse.builder()
        .policyId(policyId)
        .totalToken(tokenCount);
    scriptRepository.findByHash(policyId).ifPresent(
        script -> {
          if (StringUtils.isEmpty(script.getJson())) {
            try {
              String policyScript = objectMapper.writeValueAsString(
                  PolicyScriptResponse.builder().script("Plutus")
                      .byteCode(HexUtil.encodeHexString(script.getBytes())).build());
              policyResponse.policyScript(policyScript);
            } catch (JsonProcessingException e) {
              policyResponse.policyScript(null);
            }
          } else {
            policyResponse.policyScript(script.getJson());
          }
        }
    );
    return policyResponse.build();
  }

  @Override
  public BaseFilterResponse<TokenFilterResponse> getTokens(String policyId, Pageable pageable) {
    Page<MultiAsset> multiAssetPage = multiAssetRepository.findAllByPolicy(policyId, pageable);
    Set<String> subjects = multiAssetPage.stream().map(
        ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    var multiAssetResponsesList = multiAssetPage.map(tokenMapper::fromMultiAssetToFilterResponse);
    multiAssetResponsesList.forEach(
        ma -> ma.setMetadata(assetMetadataMapper.fromAssetMetadata(
            assetMetadataMap.get(ma.getPolicy() + ma.getName()))
        )
    );
    return new BaseFilterResponse<>(multiAssetResponsesList);
  }

  @Override
  public BaseFilterResponse<TokenAddressResponse> getHolders(String policyId, Pageable pageable) {
    List<MultiAsset> multiAssets = multiAssetRepository.findAllByPolicy(policyId);
    Page<AddressTokenProjection> multiAssetPage
        = multiAssetRepository.findAddressTokenByMultiAssetIn(multiAssets, pageable);
    Set<Long> addressIds = multiAssetPage.stream().map(AddressTokenProjection::getAddressId)
        .collect(Collectors.toSet());
    List<Address> addressList = addressRepository.findAddressByIdIn(addressIds);
    Map<Long, Address> addressMap = addressList.stream().collect(
        Collectors.toMap(Address::getId, Function.identity()));
    Page<TokenAddressResponse> tokenAddressResponses = multiAssetPage.map(
        tokenMapper::fromAddressTokenProjection);
    tokenAddressResponses.forEach(tokenAddress -> {
      tokenAddress.setAddress(
          addressMap.get(tokenAddress.getAddressId()).getAddress());
      tokenAddress.setAddressId(null);
    });
    return new BaseFilterResponse<>(tokenAddressResponses);
  }
}
