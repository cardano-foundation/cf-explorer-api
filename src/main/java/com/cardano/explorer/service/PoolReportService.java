package com.cardano.explorer.service;

import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportExportResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import org.springframework.data.domain.Pageable;

public interface PoolReportService {
    Boolean create(PoolReportCreateRequest poolReportCreateRequest);

    BaseFilterResponse<PoolReportListResponse> list(Pageable pageable);

    PoolReportDetailResponse detail(String reportId, Pageable pageable);

    BaseFilterResponse<PoolReportDetailResponse.EpochSize> detailEpochSize(String reportId, Pageable pageable);

    PoolReportExportResponse export(Long reportId);
}
