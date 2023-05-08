package org.cardanofoundation.explorer.api.service.impl;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.model.request.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.repository.ReportHistoryRepository;
import org.cardanofoundation.explorer.api.repository.StakeKeyReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReportHistoryServiceImpl implements ReportHistoryService {

  public static final String MIN_TIME = "1970-01-01 00:00:00";

  private final ReportHistoryRepository reportHistoryRepository;
  private final StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;
  private final StakeKeyReportService stakeKeyReportService;

  /**
   * Get report history
   *
   * @param filterRequest
   * @param username
   * @param pageable      pageable
   * @return BaseFilterResponse<StakeKeyReportHistoryResponse>
   */
  @Override
  public BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, String username, Pageable pageable) {
    String reportName = DataUtil.makeLikeQuery(filterRequest.getReportName());
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(Instant.now());
    if (!DataUtil.isNullOrEmpty(filterRequest.getFromDate())) {
      fromDate = Timestamp.from(filterRequest.getFromDate().toInstant());
    }
    if (!DataUtil.isNullOrEmpty(filterRequest.getToDate())) {
      toDate = Timestamp.from(filterRequest.getToDate().toInstant());
    }
    Page<ReportHistoryResponse> reportHistoryProjections = reportHistoryRepository.getRecordHistoryByFilter(
            reportName, fromDate, toDate, username, pageable)
        .map(reportHistoryProjection -> ReportHistoryResponse.builder()
            .stakeKeyReportId(reportHistoryProjection.getStakeKeyReportId())
            .poolReportId(reportHistoryProjection.getPoolReportId())
            .reportName(reportHistoryProjection.getReportName())
            .status(reportHistoryProjection.getStatus())
            .type(reportHistoryProjection.getType())
            .createdAt(reportHistoryProjection.getCreatedAt())
            .build());

    return new BaseFilterResponse<>(reportHistoryProjections);
  }

  /**
   * Get all report history that not yet persisted to storage, then persist to storage
   */
  @Scheduled(fixedDelay = 1000 * 3)
  private void persistToStorage(){
    // will be replaced by redis cache later
    List<ReportHistory> reportHistoryList = reportHistoryRepository.findByStorageKeyIsNullOrderByIdAsc();
    reportHistoryList.forEach(reportHistory -> {
      if(ReportType.STAKE_KEY.equals(reportHistory.getType())) {
        stakeKeyReportService.exportStakeKeyReport(
            stakeKeyReportHistoryRepository.findByReportHistoryId(reportHistory.getId()));
      } else {
        // TODO export pool report
      }
    });
  }
}
