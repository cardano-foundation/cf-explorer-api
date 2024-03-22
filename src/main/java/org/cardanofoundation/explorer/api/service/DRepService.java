package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;

public interface DRepService {
  BaseFilterResponse<DRepCertificateHistoryResponse> getTxDRepCertificateHistory(
      String drepHashOrDrepId, Pageable pageable);
}
