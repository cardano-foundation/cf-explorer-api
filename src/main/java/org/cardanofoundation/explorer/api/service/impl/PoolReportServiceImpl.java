package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiOsProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.*;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.report.ExcelHelper;
import org.cardanofoundation.explorer.api.util.report.ExportContent;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolReportServiceImpl implements PoolReportService {
  private static final String POOL_SIZE_TITLE = "Pool Size";
  private static final String POOL_REGISTRATIONS_TITLE = "Pool Registrations";
  private static final String POOL_UPDATE_TITLE = "Pool Update";
  private static final String REWARD_DISTRIBUTION_TITLE = "Reward Distribution";
  private static final String DEREGISTRATION_TITLE = "Deregistration";

  private final PoolReportRepository poolReportRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final StorageService storageService;

  private final PoolLifecycleService poolLifecycleService;

  private final PoolHashRepository poolHashRepository;

  private final KafkaService kafkaService;

  private final FetchRewardDataService fetchRewardDataService;

  private final PoolHistoryRepository poolHistoryRepository;

  private final EpochRepository epochRepository;

  private final Pageable defaultPageable = PageRequest.of(0, 1000, Sort.by("id").descending());

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

  @Transactional
  public void exportDirect(PoolReportHistory poolReport) {
    try {
      List<ExportContent> exportContents = getExportContents(poolReport);
      String storageKey = generateStorageKey(poolReport);
      String excelFileName = storageKey + ExportType.EXCEL.getValue();
      InputStream excelInputStream = ExcelHelper.writeContent(exportContents);
      storageService.uploadFile(excelInputStream.readAllBytes(), excelFileName);
      poolReport.getReportHistory().setStatus(ReportStatus.GENERATED);
      poolReport.getReportHistory().setStorageKey(storageKey);
      poolReportRepository.save(poolReport);
      excelInputStream.close();
    } catch (Exception e) {
      poolReport.getReportHistory().setStatus(ReportStatus.FAILED);
      log.error("Error while generating report", e);
    } finally {
      poolReportRepository.save(poolReport);
    }
  }

  private List<ExportContent> getExportContents(PoolReportHistory poolReport) {
    List<ExportContent> exportContents = new ArrayList<>();
    /// pool registrations
    if (Boolean.TRUE.equals(poolReport.getEventRegistration())) {
      var poolRegistrationBaseFilterResponse = fetchPoolRegistration(poolReport);

      exportContents.add(ExportContent.builder()
                             .clazz(PoolReportDetailResponse.PoolRegistration.class)
                             .headerTitle(POOL_REGISTRATIONS_TITLE)
                             .lstColumn(PoolReportDetailResponse.PoolRegistration.designFile())
                             .lstData(poolRegistrationBaseFilterResponse.getData())
                             .build());
    }
    // pool update
    if (Boolean.TRUE.equals(poolReport.getEventPoolUpdate())) {
      var poolUpdateBaseFilterResponse = fetchPoolUpdate(poolReport);

      exportContents.add(ExportContent.builder()
                             .clazz(PoolReportDetailResponse.PoolUpdate.class)
                             .headerTitle(POOL_UPDATE_TITLE)
                             .lstColumn(PoolReportDetailResponse.PoolUpdate.designFile())
                             .lstData(poolUpdateBaseFilterResponse.getData())
                             .build());
    }
    // reward distribution
    if (Boolean.TRUE.equals(poolReport.getEventReward())) {
      var rewardDistributionBaseFilterResponse = fetchRewardsDistribution(poolReport);

      exportContents.add(ExportContent.builder()
                             .clazz(PoolReportDetailResponse.RewardDistribution.class)
                             .headerTitle(REWARD_DISTRIBUTION_TITLE)
                             .lstColumn(PoolReportDetailResponse.RewardDistribution.designFile())
                             .lstData(rewardDistributionBaseFilterResponse.getData())
                             .build());
    }
    // deregistration
    if (Boolean.TRUE.equals(poolReport.getEventDeregistration())) {
      var deregistrationBaseFilterResponse = fetchDeregistraion(poolReport);

      exportContents.add(ExportContent.builder()
                             .clazz(PoolReportDetailResponse.Deregistration.class)
                             .headerTitle(DEREGISTRATION_TITLE)
                             .lstColumn(PoolReportDetailResponse.Deregistration.designFile())
                             .lstData(deregistrationBaseFilterResponse.getData())
                             .build());
    }
    /// epoch size
    if (Boolean.TRUE.equals(poolReport.getIsPoolSize())) {
      var epochSizeBaseFilterResponse = fetchEpochSize(poolReport);

      exportContents.add(ExportContent.builder()
                             .clazz(PoolReportDetailResponse.EpochSize.class)
                             .headerTitle(POOL_SIZE_TITLE)
                             .lstColumn(PoolReportDetailResponse.EpochSize.designFile(
                                 poolReport.getIsFeesPaid()))
                             .lstData(epochSizeBaseFilterResponse.getData())
                             .build());
    }

    return exportContents;
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

    boolean isKoiOs = fetchRewardDataService.isKoiOs();
    if (true) {
      int epochNo = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
      Set<String> poolReportSet = Set.of(poolReport.getPoolView());
      boolean isHistory = fetchRewardDataService.checkPoolHistoryForPool(poolReportSet);
      List<PoolHistoryKoiOsProjection> poolHistoryProjections = new ArrayList<>();
      if (!isHistory) {
        boolean isFetch = fetchRewardDataService.fetchPoolHistoryForPool(Set.of(poolReport.getPoolView()));
        if (isFetch) {
          poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolReportSet, epochNo - 2);
        }
      } else {
        poolHistoryProjections = poolHistoryRepository.getPoolHistoryKoiOs(poolReportSet, epochNo - 2);
      }

      if(poolHistoryProjections.size() != 0) {
        poolHistoryProjections.forEach(System.out::println);
        List<PoolReportDetailResponse.EpochSize> epochSizeList = poolHistoryProjections.stream().map(PoolReportDetailResponse.EpochSize::toDomain).toList();
        return new BaseFilterResponse<>(this.convertListToPage(epochSizeList, pageable));
      } else {
        return new BaseFilterResponse<>(null);
      }

    } else {
      Page<PoolReportProjection> epochSizeProjectionPage = epochStakeRepository.getEpochSizeByPoolReport(
              poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable);
      List<PoolReportProjection> epochSizeProjections = epochSizeProjectionPage.getContent();
      List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
              .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(epochSizeProjectionPage, epochSizes);
    }
  }

  private Page<PoolReportDetailResponse.EpochSize> convertListToPage(List<PoolReportDetailResponse.EpochSize> list, Pageable pageable) {
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

  private BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(
      PoolReportHistory poolReport) {
    List<PoolReportProjection> epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
        .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(epochSizes, epochSizeProjections.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> fetchPoolRegistration(
      PoolReportHistory poolReport) {

    List<TabularRegisResponse> tabularRegisResponses = poolLifecycleService.registrationList(
        poolReport.getPoolView(), defaultPageable).getData();
    List<PoolReportDetailResponse.PoolRegistration> poolRegistrations = tabularRegisResponses.stream()
        .map(PoolReportDetailResponse.PoolRegistration::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(poolRegistrations, poolRegistrations.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> fetchPoolUpdate(
      PoolReportHistory poolReport) {

    List<PoolUpdateDetailResponse> poolUpdateDetailResponses = poolLifecycleService.poolUpdateList(
        poolReport.getPoolView(), defaultPageable).getData();
    List<PoolReportDetailResponse.PoolUpdate> poolUpdates = poolUpdateDetailResponses.stream()
        .map(PoolReportDetailResponse.PoolUpdate::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(poolUpdates, poolUpdates.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> fetchRewardsDistribution(
      PoolReportHistory poolReport) {

    List<RewardResponse> rewardResponses = poolLifecycleService.listReward(
        poolReport.getPoolView(), defaultPageable).getData();
    List<PoolReportDetailResponse.RewardDistribution> rewardDistributions = rewardResponses.stream()
        .map(PoolReportDetailResponse.RewardDistribution::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(rewardDistributions, rewardDistributions.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.Deregistration> fetchDeregistraion(
      PoolReportHistory poolReport) {
    List<DeRegistrationResponse> deRegistrationResponses = poolLifecycleService.deRegistration(
        poolReport.getPoolView(), null, null, null, defaultPageable).getData();
    List<PoolReportDetailResponse.Deregistration> deregistrations = deRegistrationResponses.stream()
        .map(PoolReportDetailResponse.Deregistration::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(deregistrations, deregistrations.size());
  }

  private String generateStorageKey(PoolReportHistory poolReport) {
    return poolReport.getReportHistory().getId() + "_" + poolReport.getReportHistory()
        .getReportName();
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
