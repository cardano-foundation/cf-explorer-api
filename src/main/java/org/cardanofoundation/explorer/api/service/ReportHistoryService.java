package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.ReportLimitResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;

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
   * Save pool report history
   * @param poolReportHistory
   * @return PoolReportHistory
   */
  PoolReportHistory savePoolReportHistory(PoolReportHistory poolReportHistory);

  /**
   * Get pool report history by reportId and username
   * @param reportId
   * @param username
   * @return PoolReportHistory
   */
  PoolReportHistory getPoolReportHistory(Long reportId, String username);

  /**
   * Save stake key report history
   * @param stakeKeyReportHistory
   * @return StakeKeyReportHistory
   */
  StakeKeyReportHistory saveStakeKeyReportHistory(StakeKeyReportHistory stakeKeyReportHistory);

  /**
   * Get stake key report history by reportId and username
   * @param reportId
   * @param username
   * @return StakeKeyReportHistory
   */
  StakeKeyReportHistory getStakeKeyReportHistory(Long reportId, String username);
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
