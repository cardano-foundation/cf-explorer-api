package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.PolicyResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.ScriptRepository;
import com.cardano.explorer.service.PolicyService;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PolicyServiceImpl implements PolicyService {

  private final MultiAssetRepository multiAssetRepository;
  private final TokenMapper tokenMapper;
  private final ScriptRepository scriptRepository;

  @Override
  public PolicyResponse getPolicyDetail(String policyId) {
    Integer tokenCount = multiAssetRepository.countByPolicy(policyId);
    if(tokenCount.equals(0)) {
      throw new BusinessException(BusinessCode.POLICY_NOT_FOUND);
    }
    String script = scriptRepository.findJsonByHash(policyId).orElse(null);
    return PolicyResponse.builder()
        .policyId(policyId)
        .totalToken(tokenCount)
        .policyScript(script).build();
  }

  @Override
  public BaseFilterResponse<TokenFilterResponse> getTokens(String policyId, Pageable pageable) {
    Page<MultiAsset> multiAssetPage = multiAssetRepository.findAllByPolicy(policyId, pageable);
    return new BaseFilterResponse<>(multiAssetPage.map(tokenMapper::fromMultiAssetToFilterResponse));
  }

  @Override
  public BaseFilterResponse<TokenAddressResponse> getHolders(String policyId, Pageable pageable) {
    Page<AddressTokenProjection> multiAssetPage
        = multiAssetRepository.findAddressTokenByPolicy(policyId, pageable);
    return new BaseFilterResponse<>(multiAssetPage.map(tokenMapper::fromAddressTokenProjection));
  }
}
