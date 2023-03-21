package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.PolicyResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import org.springframework.data.domain.Pageable;

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


  /**
   * Get list holder by policy
   *
   * @param policyId policy hash
   * @param pageable page info
   * @return list holders in this page
   */
  BaseFilterResponse<TokenAddressResponse> getHolders(String policyId, Pageable pageable);
}
