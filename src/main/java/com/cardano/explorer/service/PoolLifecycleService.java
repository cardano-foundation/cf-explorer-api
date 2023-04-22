package com.cardano.explorer.service;

import com.cardano.explorer.model.request.pool.lifecycle.PoolUpdateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
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
}
