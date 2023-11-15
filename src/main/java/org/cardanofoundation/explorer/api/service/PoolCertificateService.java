package org.cardanofoundation.explorer.api.service;

import java.util.List;
import org.cardanofoundation.explorer.api.common.enumeration.PoolActionType;
import org.cardanofoundation.explorer.api.common.enumeration.PoolStatus;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;
import org.springframework.data.domain.Pageable;

public interface PoolCertificateService {

  BaseFilterResponse<TxPoolCertificateHistory> getTxPoolCertificateHistory(String poolViewOrHash,
      Pageable pageable);

  PoolStatus getCurrentPoolStatus(String poolViewOrHash);

  List<PoolCertificateHistory> getPoolCertificateByAction(String poolViewOrHash,
      PoolActionType action);
}