package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailListResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
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
   * @param page,size,search
   * @return BaseFilterResponse<PoolResponse>
   */
  ResponseEntity<BaseFilterResponse<PoolResponse>> getDataForPoolTable(Integer page, Integer size,
      String search);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailHeaderResponse
   */
  ResponseEntity<PoolDetailHeaderResponse> getDataForPoolDetail(Long poolId);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailListResponse
   */
  ResponseEntity<PoolDetailListResponse> getListForPoolDetail(Integer page, Integer size,
      Long poolId);

  /**
   * Get pool registration list
   *
   * @param page,size
   * @return PoolTxResponse
   */
  ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolRegistration(Integer page,
      Integer size);

  /**
   * Get pool de registration list
   *
   * @param page,size
   * @return PoolTxResponse
   */
  ResponseEntity<BaseFilterResponse<PoolTxResponse>> getDataForPoolDeRegistration(Integer page,
      Integer size);
}
