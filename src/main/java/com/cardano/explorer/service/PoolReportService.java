package com.cardano.explorer.service;

import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.lifecycle.TabularRegisResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportExportResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.sotatek.cardano.common.entity.PoolReport;
import org.springframework.data.domain.Pageable;

public interface PoolReportService {

  Boolean create(PoolReportCreateRequest poolReportCreateRequest);

  BaseFilterResponse<PoolReportListResponse> list(Pageable pageable);

  PoolReportDetailResponse detailFull(String reportId, Pageable pageable);

  BaseFilterResponse<PoolReportDetailResponse.EpochSize> detailEpochSize(String reportId,
      Pageable pageable);

  PoolReportExportResponse export(Long reportId, String fileExtension);

  BaseFilterResponse<TabularRegisResponse> detailPoolRegistration(String reportId,
      Pageable pageable);

  BaseFilterResponse<PoolUpdateDetailResponse> detailPoolUpdate(String reportId, Pageable pageable);

  BaseFilterResponse<RewardResponse> detailRewardsDistribution(String reportId, Pageable pageable);

  BaseFilterResponse<DeRegistrationResponse> detailDeregistraion(String reportId,
      Pageable pageable);

  PoolReport detail(String reportId);
}
