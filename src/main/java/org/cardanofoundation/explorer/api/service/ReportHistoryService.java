package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.ReportLimitResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;

public interface ReportHistoryService {

  /**
   * Get report history
   *
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, String username, Pageable pageable);

  /**
   * Check if the limit is reached
   * @param username
   * @return True if the limit is reached. Otherwise, return false
   */
  Boolean isLimitReached(String username);

  /**
   * Get report limit information
   * @param username
   * @return ReportLimitResponse
   */
  ReportLimitResponse getReportLimit(String username);
}
