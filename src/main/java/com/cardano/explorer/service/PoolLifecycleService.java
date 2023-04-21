package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RegistrationAllResponse;
import java.util.List;
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
}
