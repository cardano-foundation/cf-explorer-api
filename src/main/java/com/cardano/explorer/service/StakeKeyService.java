package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.StakeTxResponse;
import org.springframework.data.domain.Pageable;

public interface StakeKeyService {

  /**
   * Get stake key registration list
   *
   * @param pageable
   * @return StakeTxResponse
   */
  BaseFilterResponse<StakeTxResponse> getDataForStakeKeyRegistration(Pageable pageable);

  /**
   * Get stake key de registration list
   *
   * @param pageable
   * @return StakeTxResponse
   */
  BaseFilterResponse<StakeTxResponse> getDataForStakeKeyDeRegistration(Pageable pageable);

}
