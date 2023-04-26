package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolInfoResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.lifecycle.TabularRegisResponse;
import java.util.Date;
import org.springframework.data.domain.Pageable;

public interface PoolLifecycleService {

  /**
   * Get pool view for pool lifecycle
   *
   * @param
   * @return BaseFilterResponse<String>
   */
  BaseFilterResponse<String> getPoolViewByStakeKey(String stakeKey, Pageable pageable);

  /**
   * Get pool lifecycle registration
   *
   * @param
   * @return BaseFilterResponse<PoolUpdateResponse>
   */
  BaseFilterResponse<PoolUpdateResponse> registration(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable);

  /**
   * Get pool lifecycle registration detail
   *
   * @param
   * @return RegistrationResponse
   */
  RegistrationResponse registrationDetail(String poolView, Long id);

  /**
   * Get pool lifecycle pool update
   *
   * @param
   * @return BaseFilterResponse<PoolUpdateResponse>
   */
  BaseFilterResponse<PoolUpdateResponse> poolUpdate(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable);

  /**
   * Get pool lifecycle pool update detail
   *
   * @param
   * @return PoolUpdateDetailResponse
   */
  PoolUpdateDetailResponse poolUpdateDetail(Long id);

  /**
   * Get pool lifecycle pool reward
   *
   * @param
   * @return BaseFilterResponse<RewardResponse>
   */
  BaseFilterResponse<RewardResponse> listReward(String poolView, Pageable pageable);

  /**
   * Get pool lifecycle pool info
   *
   * @param
   * @return PoolInfoResponse
   */
  PoolInfoResponse poolInfo(String poolView);

  /**
   * Get pool lifecycle registration
   *
   * @param
   * @return BaseFilterResponse<DeRegistrationResponse>
   */
  BaseFilterResponse<DeRegistrationResponse> deRegistration(String poolView, String txHash,
      Date fromDate, Date toDate,
      Pageable pageable);

  /**
   * Get pool lifecycle registration list
   *
   * @param
   * @return BaseFilterResponse<TabularRegisResponse>
   */
  BaseFilterResponse<TabularRegisResponse> registrationList(String poolView, Pageable pageable);


  /**
   * Get pool lifecycle pool update list
   *
   * @param
   * @return BaseFilterResponse<PoolUpdateDetailResponse>
   */
  BaseFilterResponse<PoolUpdateDetailResponse> poolUpdateList(String poolView, Pageable pageable);

}
