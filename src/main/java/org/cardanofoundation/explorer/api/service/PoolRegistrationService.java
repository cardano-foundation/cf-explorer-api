package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;

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
