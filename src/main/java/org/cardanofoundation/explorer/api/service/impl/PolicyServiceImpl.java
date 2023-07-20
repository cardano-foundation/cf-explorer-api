package org.cardanofoundation.explorer.api.service.impl;

import com.bloxbean.cardano.client.util.HexUtil;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyScriptResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.service.PolicyService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PolicyServiceImpl implements PolicyService {

  private final MultiAssetRepository multiAssetRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressRepository addressRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
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
    Page<AddressTokenProjection> multiAssetPage
        = addressTokenBalanceRepository.findAddressAndBalanceByMultiAssetIn(policyId, pageable);
    Set<Long> addressIds = multiAssetPage.stream().map(AddressTokenProjection::getAddressId)
        .collect(Collectors.toSet());
    List<Address> addressList = addressRepository.findAddressByIdIn(addressIds);
    Map<Long, Address> addressMap = addressList.stream().collect(
        Collectors.toMap(Address::getId, Function.identity()));
    Page<TokenAddressResponse> tokenAddressResponses = multiAssetPage.map(
        tokenMapper::fromAddressTokenProjection);
    Set<String> subjects = multiAssetPage.stream().map(
        ma -> ma.getPolicy() + ma.getTokenName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    tokenAddressResponses.forEach(tokenAddress -> {
      tokenAddress.setAddress(
          addressMap.get(tokenAddress.getAddressId()).getAddress());
      tokenAddress.setMetadata(assetMetadataMapper.fromAssetMetadata(
          assetMetadataMap.get(tokenAddress.getPolicy() + tokenAddress.getName())));
      tokenAddress.setAddressId(null);
    });
    return new BaseFilterResponse<>(tokenAddressResponses);
  }
}
