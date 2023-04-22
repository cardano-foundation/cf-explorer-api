package com.cardano.explorer.service;

import com.cardano.explorer.model.request.pool.lifecycle.PoolUpdateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolInfoResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
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
   * @return RegistrationAllResponse
   */
  RegistrationAllResponse registration(String poolView);

  /**
   * Get pool lifecycle pool update
   *
   * @param
   * @return BaseFilterResponse<PoolUpdateResponse>
   */
  BaseFilterResponse<PoolUpdateResponse> poolUpdate(PoolUpdateRequest poolUpdateRequest,
      Pageable pageable);

  /**
   * Get pool lifecycle pool update detail
   *
   * @param
   * @return PoolUpdateDetailResponse
   */
  PoolUpdateDetailResponse poolUpdateDetail(Long id, Long previousId);

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
   * @return RegistrationAllResponse
   */
  DeRegistrationAllResponse deRegistration(String poolView);
}
