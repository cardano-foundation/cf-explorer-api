package com.cardano.explorer.service;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.PoolTxResponse;
import org.springframework.data.domain.Pageable;

public interface PoolRegistrationService {

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
}
