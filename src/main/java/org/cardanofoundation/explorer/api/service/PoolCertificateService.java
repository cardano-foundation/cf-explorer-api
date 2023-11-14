package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.PoolStatus;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;

public interface PoolCertificateService {
  BaseFilterResponse<TxPoolCertificateHistory> getTxPoolCertificateHistory(String poolViewOrHash,
                                                                           Pageable pageable);

  PoolStatus getCurrentPoolStatus(String poolViewOrHash);
}