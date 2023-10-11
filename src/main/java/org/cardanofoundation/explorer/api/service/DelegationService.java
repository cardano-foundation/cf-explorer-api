package org.cardanofoundation.explorer.api.service;

import java.util.List;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.springframework.data.domain.Pageable;

public interface DelegationService {


  /**
   * Get list of delegations
   *
   * @param pageable page, size and sort parameters
   * @return list delegation transaction information
   */
  BaseFilterResponse<DelegationResponse> getDelegations(Pageable pageable);

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
   * @param poolViewOrHash
   * @return PoolDetailHeaderResponse
   */
  PoolDetailHeaderResponse getDataForPoolDetail(String poolViewOrHash);

  /**
   * Get detail pool epoch list for delegate pools detail
   *
   * @param poolViewOrHash
   * @return PoolDetailEpochResponse
   */
  BaseFilterResponse<PoolDetailEpochResponse> getEpochListForPoolDetail(Pageable pageable,
      String poolViewOrHash);

  /**
   * Get detail pool analytics for delegate pools detail
   *
   * @param poolViewOrHash
   * @return PoolDetailAnalyticsResponse
   */
  PoolDetailAnalyticsResponse getAnalyticsForPoolDetail(String poolViewOrHash);

  /**
   * Get detail pool delegator list for delegate pools detail
   *
   * @param poolViewOrHash, pageable
   * @return PoolDetailDelegatorsResponse
   */
  BaseFilterResponse<PoolDetailDelegatorResponse> getDelegatorsForPoolDetail(Pageable pageable,
      String poolViewOrHash);

  /**
   * Find Top {number} (default 3) Pool Delegation
   *
   * @param pageable page information
   * @return
   */
  List<PoolResponse> findTopDelegationPool(Pageable pageable);
}
