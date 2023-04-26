package com.cardano.explorer.service;

import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportResponse;
import org.springframework.data.domain.Pageable;

public interface StakeKeyReportService extends StorageService {

  StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReport stakeKeyReport);

  BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(String stakeKey,
      Pageable pageable);

  StakeKeyReportResponse exportStakeKeyReport(Long reportId);

}
