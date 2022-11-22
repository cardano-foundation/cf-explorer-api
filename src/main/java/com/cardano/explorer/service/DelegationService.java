package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.DelegationHeaderResponse;
import com.cardano.explorer.model.response.PoolDetailResponse;
import com.cardano.explorer.model.response.PoolResponse;
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
  ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(Integer page, Integer size,
      String search);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailResponse
   */
  ResponseEntity<PoolDetailResponse> getDataForPoolDetail(Long poolId);
}
