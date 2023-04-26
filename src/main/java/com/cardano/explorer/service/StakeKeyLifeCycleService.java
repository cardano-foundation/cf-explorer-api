package com.cardano.explorer.service;

import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.springframework.data.domain.Pageable;

public interface StakeKeyLifeCycleService {

  /**
   * Get list registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key registration
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);

  /**
   * Get list de registration in stake key life cycle
   * @param stakeKey stake address view
   * @param condition condition filter
   * @param pageable page information
   * @return list stake key de registration
   */
  BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest condition, Pageable pageable);
}
