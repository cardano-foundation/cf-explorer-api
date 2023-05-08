package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.report.ReportHistoryResponse;

public interface ReportHistoryService {

  /**
   * Get report history
   *
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, String username, Pageable pageable);

}
