package org.cardanofoundation.explorer.api.service.impl;

import static org.cardanofoundation.explorer.api.service.impl.ReportHistoryServiceImpl.MIN_TIME;

import java.io.ByteArrayInputStream;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ValueRange;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.interceptor.auth.UserPrincipal;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.repository.explorer.PoolReportRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryRepository;
import org.cardanofoundation.explorer.api.service.*;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.common.entity.enumeration.ReportStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.ReportType;
import org.cardanofoundation.explorer.common.entity.explorer.PoolReportHistory;
import org.cardanofoundation.explorer.common.entity.explorer.ReportHistory;
import org.cardanofoundation.explorer.common.exception.BusinessException;
import org.cardanofoundation.explorer.common.exception.CommonErrorCode;
import org.cardanofoundation.explorer.common.model.ReportMessage;

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

  private final FetchRewardDataService fetchRewardDataService;

  private final PoolHistoryRepository poolHistoryRepository;

  private final ReportHistoryService reportHistoryService;

  private final RoleService roleService;

  @Override
  public Boolean create(
      PoolReportCreateRequest poolReportCreateRequest, UserPrincipal userPrincipal) {
    PoolReportHistory poolReportHistory = saveToDb(poolReportCreateRequest, userPrincipal);

    ReportMessage reportMessage =
        ReportMessage.builder()
            .reportHistory(poolReportHistory.getReportHistory())
            .timePattern(poolReportCreateRequest.getTimePattern())
            .zoneOffset(poolReportCreateRequest.getZoneOffset())
            //            .dateFormat(poolReportCreateRequest.getDateFormat())
            .build();

    Boolean isSuccess = kafkaService.sendReportHistory(reportMessage);
    if (Boolean.FALSE.equals(isSuccess)) {
      poolReportRepository.delete(poolReportHistory);
      throw new BusinessException(CommonErrorCode.UNKNOWN_ERROR);
    }

    return true;
  }

  @Transactional
  public PoolReportHistory saveToDb(
      PoolReportCreateRequest poolReportCreateRequest, UserPrincipal userPrincipal) {
    poolHashRepository
        .findByViewOrHashRaw(poolReportCreateRequest.getPoolId())
        .orElseThrow(() -> new BusinessException(BusinessCode.POOL_NOT_FOUND));
    int reportLimit = roleService.getReportLimit(userPrincipal.getRoleDescription());
    if (reportLimit != CommonConstant.UNLIMITED_REPORT) { // "-1" it's mean unlimited report
      if (Boolean.TRUE.equals(
          reportHistoryService.isLimitReached(userPrincipal.getUsername(), reportLimit))) {
        throw new BusinessException(BusinessCode.REPORT_LIMIT_REACHED);
      }
    }
    ReportHistory reportHistory =
        initReportHistory(poolReportCreateRequest, userPrincipal.getUsername());
    return poolReportRepository.saveAndFlush(poolReportCreateRequest.toEntity(reportHistory));
  }

  @Override
  public PoolReportExportResponse export(Long reportId, ExportType exportType, String username) {
    if (!ExportType.EXCEL.equals(exportType)) {
      throw new BusinessException(BusinessCode.EXPORT_TYPE_NOT_SUPPORTED);
    }

    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);
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
  public BaseFilterResponse<PoolReportListResponse> list(
      Pageable pageable, String username, ReportHistoryFilterRequest filterRequest) {

    Timestamp timeAt7DayAgo = new Timestamp(Instant.now().minus(Duration.ofDays(7)).toEpochMilli());
    String reportName = DataUtil.makeLikeQuery(filterRequest.getReportName());
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(Instant.now());
    if (!DataUtil.isNullOrEmpty(filterRequest.getFromDate())) {
      fromDate = Timestamp.from(filterRequest.getFromDate().toInstant());
    }
    if (!DataUtil.isNullOrEmpty(filterRequest.getToDate())) {
      toDate = Timestamp.from(filterRequest.getToDate().toInstant());
    }

    Page<PoolReportHistory> poolReportPage =
        poolReportRepository.getPoolReportHistoryByFilter(
            reportName, fromDate, toDate, username, pageable);
    List<PoolReportHistory> poolReports = poolReportPage.getContent();
    List<PoolReportListResponse> poolReportListResponses =
        poolReports.stream()
            .map(
                poolReportHistory -> {
                  PoolReportListResponse response =
                      PoolReportListResponse.toDomain(poolReportHistory);

                  if (response.getCreatedAt().before(timeAt7DayAgo)) {
                    response.setStatus(ReportStatus.EXPIRED);
                  }
                  return response;
                })
            .toList();
    return new BaseFilterResponse<>(poolReportPage, poolReportListResponses);
  }

  @Override
  public PoolReportHistory detail(Long reportId, String username) {
    return getPoolReportHistory(reportId, username);
  }

  @Override
  public BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(
      Long reportId, Pageable pageable, String username) {
    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);

    boolean useKoios = fetchRewardDataService.useKoios();
    if (useKoios) {
      Set<String> poolReportSet = Set.of(poolReport.getPoolView());
      boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolReportSet);
      List<PoolHistoryKoiosProjection> poolHistoryProjections = new ArrayList<>();
      if (!isHistory) {
        boolean isFetch =
            fetchRewardDataService.fetchPoolHistoryForPool(Set.of(poolReport.getPoolView()));
        if (isFetch) {
          poolHistoryProjections =
              poolHistoryRepository.getPoolHistoryKoios(poolReport.getPoolView());
        }
      } else {
        poolHistoryProjections =
            poolHistoryRepository.getPoolHistoryKoios(poolReport.getPoolView());
      }

      if (Objects.nonNull(poolHistoryProjections)) {
        List<PoolReportDetailResponse.EpochSize> epochSizeList =
            poolHistoryProjections.stream()
                .filter(
                    t ->
                        ValueRange.of(poolReport.getBeginEpoch(), poolReport.getEndEpoch())
                            .isValidIntValue(t.getEpochNo()))
                .map(PoolReportDetailResponse.EpochSize::toDomain)
                .toList();
        return new BaseFilterResponse<>(this.convertListToPage(epochSizeList, pageable));
      } else {
        return new BaseFilterResponse<>();
      }

    } else {
      return new BaseFilterResponse<>();
    }
  }

  private Page<PoolReportDetailResponse.EpochSize> convertListToPage(
      List<PoolReportDetailResponse.EpochSize> list, Pageable pageable) {
    int startIndex = pageable.getPageNumber() * pageable.getPageSize();
    int endIndex = Math.min(startIndex + pageable.getPageSize(), list.size());
    try {
      List<PoolReportDetailResponse.EpochSize> sublist = list.subList(startIndex, endIndex);
      PageRequest pageRequest = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());
      return new PageImpl<>(sublist, pageRequest, list.size());
    } catch (Exception e) {
      return new PageImpl<>(new ArrayList<>());
    }
  }

  @Override
  public BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(
      Long reportId, Pageable pageable, String username) {
    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);
    return poolLifecycleService.registrationList(poolReport.getPoolView(), pageable);
  }

  @Override
  public BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(
      Long reportId, Pageable pageable, String username) {
    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);
    return poolLifecycleService.poolUpdateList(poolReport.getPoolView(), pageable);
  }

  @Override
  public BaseFilterResponse<RewardResponse> fetchRewardsDistribution(
      Long reportId, Pageable pageable, String username) {
    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);
    if (Boolean.FALSE.equals(fetchRewardDataService.useKoios())) {
      return new BaseFilterResponse<>();
    }

    return poolLifecycleService.listRewardFilter(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable);
  }

  @Override
  public BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(
      Long reportId, Pageable pageable, String username) {
    PoolReportHistory poolReport = getPoolReportHistory(reportId, username);
    return poolLifecycleService.deRegistration(
        poolReport.getPoolView(), null, null, null, pageable);
  }

  private PoolReportHistory getPoolReportHistory(Long reportId, String username) {
    PoolReportHistory poolReportHistory =
        poolReportRepository
            .findById(reportId)
            .orElseThrow(() -> new BusinessException(BusinessCode.POOL_REPORT_HISTORY_NOT_FOUND));

    if (DataUtil.isNullOrEmpty(username)
        || !username.equals(poolReportHistory.getReportHistory().getUsername())) {
      throw new BusinessException(BusinessCode.POOL_REPORT_HISTORY_NOT_FOUND);
    }
    return poolReportHistory;
  }

  private ReportHistory initReportHistory(
      PoolReportCreateRequest poolReportCreateRequest, String username) {
    return ReportHistory.builder()
        .reportName(
            DataUtil.isNullOrEmpty(poolReportCreateRequest.getReportName())
                ? generateReportName(poolReportCreateRequest)
                : poolReportCreateRequest.getReportName())
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.POOL_ID)
        .username(username)
        .createdAt(Timestamp.valueOf(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)))
        .build();
  }

  private String generateReportName(PoolReportCreateRequest poolReportCreateRequest) {
    return "report_pool_"
        + poolReportCreateRequest.getPoolId()
        + "_"
        + poolReportCreateRequest.getEpochRanges()[0]
        + "_"
        + poolReportCreateRequest.getEpochRanges()[1];
  }
}
