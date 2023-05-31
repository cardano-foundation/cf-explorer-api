package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolReportRepository;
import org.cardanofoundation.explorer.api.service.KafkaService;
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.api.service.StorageService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

import java.io.ByteArrayInputStream;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolReportServiceImpl implements PoolReportService {

  private final PoolReportRepository poolReportRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final StorageService storageService;

  private final PoolLifecycleService poolLifecycleService;

  private final PoolHashRepository poolHashRepository;
  private final KafkaService kafkaService;

  @Override
  public Boolean create(PoolReportCreateRequest poolReportCreateRequest, String username) {
    PoolReportHistory poolReportHistory = saveToDb(poolReportCreateRequest, username);
    kafkaService.sendReportHistory(poolReportHistory.getReportHistory());
    return true;
  }

  @Transactional
  public PoolReportHistory saveToDb(PoolReportCreateRequest poolReportCreateRequest, String username) {
    poolHashRepository.findByView(poolReportCreateRequest.getPoolId())
        .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND));

    ReportHistory reportHistory = initReportHistory(poolReportCreateRequest, username);
    return poolReportRepository.saveAndFlush(poolReportCreateRequest.toEntity(reportHistory));
  }

  @Override
  public PoolReportExportResponse export(Long reportId, ExportType exportType, String username) {
    if (!ExportType.EXCEL.equals(exportType)) {
      throw new BusinessException(BusinessCode.EXPORT_TYPE_NOT_SUPPORTED);
    }

    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username, reportId);
    String storageKey = poolReport.getReportHistory().getStorageKey();
    String reportName = poolReport.getReportHistory().getReportName();
    ReportStatus reportStatus = poolReport.getReportHistory().getStatus();

    if (DataUtil.isNullOrEmpty(storageKey) || ReportStatus.IN_PROGRESS.equals(reportStatus)) {
      throw new BusinessException(BusinessCode.REPORT_IS_IN_PROGRESS);
    } else {
      byte[] bytes = storageService.downloadFile(storageKey + exportType.getValue());
      return PoolReportExportResponse.builder()
          .fileName(reportName + exportType.getValue())
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    }
  }

  @Override
  public BaseFilterResponse<PoolReportListResponse> list(Pageable pageable, String username) {

    Timestamp timeAt7DayAgo = new Timestamp(Instant.now().minus(Duration.ofDays(7)).toEpochMilli());
    Page<PoolReportHistory> poolReportPage = poolReportRepository.findByUsername(username,
                                                                                 pageable);
    List<PoolReportHistory> poolReports = poolReportPage.getContent();
    List<PoolReportListResponse> poolReportListResponses = poolReports.stream()
        .map(poolReportHistory -> {
          PoolReportListResponse response = PoolReportListResponse
              .toDomain(poolReportHistory);

          if(response.getCreatedAt().before(timeAt7DayAgo)) {
            response.setStatus(ReportStatus.EXPIRED);
          }
          return response;
        })
        .toList();
    return new BaseFilterResponse<>(poolReportPage, poolReportListResponses);
  }

  @Override
  public PoolReportHistory detail(Long reportId, String username) {
    return poolReportRepository.findByUsernameAndId(username, reportId);
  }

  @Override
  public BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(Long reportId,
                                                                               Pageable pageable,
                                                                               String username) {
    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username,
                                                                            reportId);
    Page<PoolReportProjection> epochSizeProjectionPage = epochStakeRepository.getEpochSizeByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable);
    List<PoolReportProjection> epochSizeProjections = epochSizeProjectionPage.getContent();
    List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
        .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(epochSizeProjectionPage, epochSizes);
  }

  @Override
  public BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(Long reportId,
                                                                        Pageable pageable,
                                                                        String username) {
    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username,
                                                                            reportId);
    return poolLifecycleService.registrationList(poolReport.getPoolView(), pageable);
  }

  @Override
  public BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(Long reportId,
                                                                      Pageable pageable,
                                                                      String username) {
    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username,
                                                                            reportId);
    return poolLifecycleService.poolUpdateList(poolReport.getPoolView(), pageable);
  }

  @Override
  public BaseFilterResponse<RewardResponse> fetchRewardsDistribution(Long reportId,
                                                                     Pageable pageable,
                                                                     String username) {
    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username,
                                                                            reportId);
    return poolLifecycleService.listReward(poolReport.getPoolView(), pageable);
  }

  @Override
  public BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(Long reportId,
                                                                       Pageable pageable,
                                                                       String username) {
    PoolReportHistory poolReport = poolReportRepository.findByUsernameAndId(username,
                                                                            reportId);
    return poolLifecycleService.deRegistration(poolReport.getPoolView(), null, null, null,
                                               pageable);
  }

  private ReportHistory initReportHistory(PoolReportCreateRequest poolReportCreateRequest,
                                          String username) {
    return ReportHistory.builder()
        .reportName(DataUtil.isNullOrEmpty(poolReportCreateRequest.getReportName())
                    ? generateReportName(poolReportCreateRequest)
                    : poolReportCreateRequest.getReportName())
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.POOL_ID)
        .username(username)
        .createdAt(Timestamp.valueOf(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)))
        .build();
  }

  private String generateReportName(PoolReportCreateRequest poolReportCreateRequest) {
    return "report_pool_" + poolReportCreateRequest.getPoolId() + "_"
        + poolReportCreateRequest.getEpochRanges()[0] + "_"
        + poolReportCreateRequest.getEpochRanges()[1];
  }
}
