package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;

import org.springframework.data.domain.Pageable;

public interface PoolReportService {


  /**
   * Generate pool report of current user
   *
   * @param poolReportCreateRequest request body
   * @param username                username of current request user
   * @return true if success
   */
  Boolean create(PoolReportCreateRequest poolReportCreateRequest, String username);


  /**
   * Download the export file of pool report from storage of current user
   *
   * @param reportId   report id
   * @param exportType export type
   * @param username   username
   * @return PoolReportExportResponse
   */
  PoolReportExportResponse export(Long reportId, ExportType exportType, String username);

  /**
   * Get all pool report history by username and filter by {@link ReportHistoryFilterRequest}
   *
   * @param pageable pageable
   * @param username username
   * @param filterRequest filter request
   * @return BaseFilterResponse<PoolReportListResponse>
   */
  BaseFilterResponse<PoolReportListResponse> list(Pageable pageable, String username, ReportHistoryFilterRequest filterRequest);

  /**
   * Get pool report detail by report id
   *
   * @param reportId report id
   * @param username username
   * @return PoolReportDetailResponse
   */
  PoolReportHistory detail(Long reportId, String username);

  /**
   * Get pool size detail by report id
   *
   * @param reportId report id
   * @param pageable pageable
   * @param username username
   * @return PoolReportDetailResponse
   */
  BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(Long reportId,
                                                                        Pageable pageable,
                                                                        String username);
  /**
   * Get pool registration detail by report id
   *
   * @param reportId report id
   * @param pageable pageable
   * @param username username
   * @return PoolReportDetailResponse
   */
  BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(Long reportId, Pageable pageable,
                                                                 String username);

  /**
   * Get pool update detail by report id
   *
   * @param reportId report id
   * @param pageable pageable
   * @param username username
   * @return PoolReportDetailResponse
   */
  BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(Long reportId, Pageable pageable,
                                                               String username);

  /**
   * Get rewards distribution detail by report id
   *
   * @param reportId report id
   * @param pageable pageable
   * @param username username
   * @return PoolReportDetailResponse
   */
  BaseFilterResponse<RewardResponse> fetchRewardsDistribution(Long reportId, Pageable pageable,
                                                              String username);

  /**
   * Get pool deregistration detail by report id
   *
   * @param reportId report id
   * @param pageable pageable
   * @param username username
   * @return PoolReportDetailResponse
   */
  BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(Long reportId, Pageable pageable,
                                                                String username);
}
