package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import com.cardano.explorer.model.response.pool.DelegationHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolDetailEpochResponse;
import com.cardano.explorer.model.response.pool.PoolDetailHeaderResponse;
import com.cardano.explorer.model.response.pool.PoolResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import com.cardano.explorer.model.response.pool.chart.PoolDetailAnalyticsResponse;
import java.util.Set;
import org.springframework.data.domain.Pageable;

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
   * @param pageable,search
   * @return BaseFilterResponse<PoolResponse>
   */
  BaseFilterResponse<PoolResponse> getDataForPoolTable(Pageable pageable, String search);

  /**
   * Get detail pool for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailHeaderResponse
   */
  PoolDetailHeaderResponse getDataForPoolDetail(String poolView);

  /**
   * Get detail pool epoch list for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailEpochResponse
   */
  BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolView);

  /**
   * Get pool registration list
   *
   * @param pageable
   * @return PoolTxResponse
   */
  BaseFilterResponse<PoolTxResponse> getDataForPoolRegistration(Pageable pageable);

  /**
   * Get pool de registration list
   *
   * @param pageable
   * @return PoolTxResponse
   */
  BaseFilterResponse<PoolTxResponse> getDataForPoolDeRegistration(Pageable pageable);

  /**
   * Get detail pool analytics for delegate pools detail
   *
   * @param poolId
   * @return PoolDetailAnalyticsResponse
   */
  PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String poolView);

  /**
   * Get detail pool delegator list for delegate pools detail
   *
   * @param poolId,pageable
   * @return PoolDetailDelegatorsResponse
   */
  BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(Pageable pageable,
      String poolView);

  /**
   * Find Top {number} (default 3) Pool Delegation
   *
   * @param pageable page information
   * @return
   */
  Set<PoolResponse> findTopDelegationPool(Pageable pageable);
}
