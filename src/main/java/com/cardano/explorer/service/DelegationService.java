package com.cardano.explorer.service;

import com.cardano.explorer.model.request.DelegationFilterRequest;
import com.cardano.explorer.model.response.DelegationHeaderResponse;
import com.cardano.explorer.model.response.PoolDetailResponse;
import com.cardano.explorer.model.response.PoolResponse;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

public interface DelegationService {

  /**
   * Get data pool for header delegate pools
   *
   * @param
   * @return DelegationHeaderResponse
   */
  ResponseEntity<DelegationHeaderResponse> getDataForDelegationHeader();

  /**
   * Get list pool for delegate pools
   *
   * @param delegationFilterRequest
   * @return Page<PoolResponse>
   */
  ResponseEntity<Page<PoolResponse>> getDataForPoolTable(
      DelegationFilterRequest delegationFilterRequest);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailResponse
   */
  ResponseEntity<PoolDetailResponse> getDataForPoolDetail(Long poolId);
}
