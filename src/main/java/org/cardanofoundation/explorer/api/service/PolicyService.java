package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;

public interface PolicyService {

  /**
   * Get policy information detail
   *
   * @param policyId policy hash
   * @return policy detail info
   */
  PolicyResponse getPolicyDetail(String policyId);

  /**
   * Get list token by policy
   *
   * @param policyId policy hash
   * @param pageable page info
   * @return list token in this page
   */
  BaseFilterResponse<TokenFilterResponse> getTokens(String policyId, Pageable pageable);
}
