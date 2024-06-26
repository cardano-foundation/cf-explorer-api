package org.cardanofoundation.explorer.api.service.impl;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.bloxbean.cardano.client.util.HexUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyScriptResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.service.PolicyService;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.AssetMetadata;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
public class PolicyServiceImpl implements PolicyService {

  private final MultiAssetRepository multiAssetRepository;
  private final AssetMetadataRepository assetMetadataRepository;
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
    var policyResponse = PolicyResponse.builder().policyId(policyId).totalToken(tokenCount);
    scriptRepository
        .findByHash(policyId)
        .ifPresent(
            script -> {
              if (StringUtils.isEmpty(script.getJson())) {
                try {
                  String policyScript =
                      objectMapper.writeValueAsString(
                          PolicyScriptResponse.builder()
                              .script("Plutus")
                              .byteCode(HexUtil.encodeHexString(script.getBytes()))
                              .build());
                  policyResponse.policyScript(policyScript);
                } catch (JsonProcessingException e) {
                  policyResponse.policyScript(null);
                }
              } else {
                policyResponse.policyScript(script.getJson());
              }
              if (ScriptType.TIMELOCK.equals(script.getType())) {
                policyResponse.isNativeScript(true);
                policyResponse.isSmartContract(false);
              } else {
                policyResponse.isSmartContract(true);
                policyResponse.isNativeScript(false);
              }
            });
    return policyResponse.build();
  }

  @Override
  public BaseFilterResponse<TokenFilterResponse> getTokens(String policyId, Pageable pageable) {
    Page<MultiAsset> multiAssetPage = multiAssetRepository.findAllByPolicy(policyId, pageable);
    Set<String> subjects =
        multiAssetPage.stream()
            .map(ma -> ma.getPolicy() + ma.getName())
            .collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap =
        assetMetadataList.stream()
            .collect(Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    var multiAssetResponsesList = multiAssetPage.map(tokenMapper::fromMultiAssetToFilterResponse);
    multiAssetResponsesList.forEach(
        ma ->
            ma.setMetadata(
                assetMetadataMapper.fromAssetMetadata(
                    assetMetadataMap.get(ma.getPolicy() + ma.getName()))));
    return new BaseFilterResponse<>(multiAssetResponsesList);
  }
}
