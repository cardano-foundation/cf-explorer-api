package org.cardanofoundation.explorer.api.service.impl;

import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.ReportLimitResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.repository.ReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReportHistoryServiceImpl implements ReportHistoryService {

  public static final String MIN_TIME = "1970-01-01 00:00:00";

  private final ReportHistoryRepository reportHistoryRepository;

  @Value("${application.report.limit-per-24hours}")
  private Integer limitPer24Hours;

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

    LocalDateTime timeAt7DayAgo = LocalDateTime.now().minus(Duration.ofDays(7));

    Page<ReportHistoryResponse> reportHistoryProjections = reportHistoryRepository.getRecordHistoryByFilter(
            reportName, fromDate, toDate, username, pageable)
        .map(reportHistoryProjection -> {
          ReportHistoryResponse response = ReportHistoryResponse.builder()
              .stakeKeyReportId(reportHistoryProjection.getStakeKeyReportId())
              .poolReportId(reportHistoryProjection.getPoolReportId())
              .reportName(reportHistoryProjection.getReportName())
              .status(reportHistoryProjection.getStatus())
              .type(reportHistoryProjection.getType())
              .createdAt(reportHistoryProjection.getCreatedAt())
              .build();
          if(response.getCreatedAt().isBefore(timeAt7DayAgo)) {
            response.setStatus(ReportStatus.EXPIRED);
          }
          return response;
        });

    return new BaseFilterResponse<>(reportHistoryProjections);
  }

  @Override
  public Boolean isLimitReached(String username) {
    Instant currentTime = Instant.now();
    Integer reportCount = reportHistoryRepository
        .countByUsernameAndCreatedAtBetween(username,
                                            Timestamp.from(currentTime.minus(Duration.ofDays(1))),
                                            Timestamp.from(currentTime));
    return reportCount >= limitPer24Hours;
  }

  @Override
  public ReportLimitResponse getReportLimit(String username) {
    return ReportLimitResponse.builder()
        .limitPer24hours(limitPer24Hours)
        .isLimitReached(isLimitReached(username))
        .build();
  }
}
