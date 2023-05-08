package org.cardanofoundation.explorer.api.service;

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

  Boolean create(PoolReportCreateRequest poolReportCreateRequest);

  BaseFilterResponse<PoolReportListResponse> list(Pageable pageable);

  PoolReportDetailResponse detailFull(String reportId, Pageable pageable);

  BaseFilterResponse<PoolReportDetailResponse.EpochSize> detailEpochSize(String reportId,
      Pageable pageable);

  PoolReportExportResponse export(Long reportId, String fileExtension);

  BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(String reportId,
      Pageable pageable);

  BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(String reportId, Pageable pageable);

  BaseFilterResponse<RewardResponse> fetchRewardsDistribution(String reportId, Pageable pageable);

  BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(String reportId,
      Pageable pageable);

  PoolReport detail(String reportId);
}
