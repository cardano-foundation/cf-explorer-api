package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.springframework.data.domain.Pageable;

public interface PoolReportService {

  Boolean create(PoolReportCreateRequest poolReportCreateRequest, String username);

  PoolReportExportResponse export(Long reportId, ExportType exportType, String username);

  void exportDirect(PoolReport poolReport);

  BaseFilterResponse<PoolReportListResponse> list(Pageable pageable, String username);

  PoolReport detail(String reportId, String username);

  PoolReportDetailResponse detailFull(String reportId, Pageable pageable, String username);

  BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(String reportId, Pageable pageable, String username);

  BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(String reportId, Pageable pageable, String username);

  BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(String reportId, Pageable pageable, String username);

  BaseFilterResponse<RewardResponse> fetchRewardsDistribution(String reportId, Pageable pageable, String username);

  BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(String reportId, Pageable pageable, String username);
}
