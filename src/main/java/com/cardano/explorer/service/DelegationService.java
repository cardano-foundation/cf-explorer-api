package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;

public interface DelegationService {

  /**
   * Get data pool for header delegate pools
   *
   * @param
   * @return DelegationHeaderResponse
   */
  DelegationHeaderResponse getDataForDelegationHeader();

  /**
   * Get list pool for delegate pools
   *
   * @param page,size,search
   * @return BaseFilterResponse<PoolResponse>
   */
  BaseFilterResponse<PoolResponse> getDataForPoolTable(Integer page, Integer size, String search);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailHeaderResponse
   */
  PoolDetailHeaderResponse getDataForPoolDetail(Long poolId);

  /**
   * Get detail pool epoch list for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailEpochResponse
   */
  BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Integer page, Integer size,
      Long poolId);

  /**
   * Get pool registration list
   *
   * @param page,size
   * @return PoolTxResponse
   */
  BaseFilterResponse<PoolTxResponse> getDataForPoolRegistration(Integer page, Integer size);

  /**
   * Get pool de registration list
   *
   * @param page,size
   * @return PoolTxResponse
   */
  BaseFilterResponse<PoolTxResponse> getDataForPoolDeRegistration(Integer page, Integer size);

  /**
   * Get detail pool analytics for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailAnalyticsResponse
   */
  PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(Long poolId);

  /**
   * Get detail pool delegator list for delegate pools detail
   *
   * @param poolId,page,size
   * @return PoolDetailDelegatorsResponse
   */
  BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(Integer page,
      Integer size, Long poolId);
}
