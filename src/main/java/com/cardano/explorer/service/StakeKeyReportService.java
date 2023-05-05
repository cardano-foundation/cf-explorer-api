package com.cardano.explorer.service;

import com.cardano.explorer.model.request.report.ReportHistoryFilterRequest;
import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.report.ReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportResponse;
import org.springframework.data.domain.Pageable;

/**
 * StakeKeyReportService interface for stake key report
 */
public interface StakeKeyReportService extends StorageService {

  /**
   * Generate stake key report
   * @param stakeKeyReport stake key report
   * @return StakeKeyReportHistoryResponse
   */
  StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReport stakeKeyReport);


  /**
   * Get stake key report history
   * @param stakeKey stake key
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistoryByStakeKey(String stakeKey,
      Pageable pageable);


  /**
   * Get all stake key report history
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(Pageable pageable);

  /**
   * Get report history
   * @param pageable pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, Pageable pageable);

  /**
   * Export stake key report by report id
   * @param reportId report id
   * @return StakeKeyReportResponse
   */
  StakeKeyReportResponse exportStakeKeyReport(Long reportId, String fileExtension);

}
