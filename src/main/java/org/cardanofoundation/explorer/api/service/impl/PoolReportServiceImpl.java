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
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.api.service.StorageService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.report.CSVHelper;
import org.cardanofoundation.explorer.api.util.report.ExcelHelper;
import org.cardanofoundation.explorer.api.util.report.ExportContent;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolReportServiceImpl implements PoolReportService {

  public static final String EPOCH_SIZE_TITLE = "Epoch Size";
  public static final String POOL_REGISTRATIONS_TITLE = "Pool Registrations";
  public static final String POOL_UPDATE_TITLE = "Pool Update";
  public static final String REWARD_DISTRIBUTION_TITLE = "Reward Distribution";
  public static final String DEREGISTRATION_TITLE = "Deregistration";
  public static final String CSV_EXTENSION = ".csv";
  public static final String EXCEL_EXTENSION = ".xlsx";

  private final PoolReportRepository poolReportRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final StorageService storageService;

  private final PoolLifecycleService poolLifecycleService;

  private final PoolHashRepository poolHashRepository;

  @Override
  public Boolean create(PoolReportCreateRequest poolReportCreateRequest, String username) throws BusinessException {
    try {
      poolHashRepository.findByView(poolReportCreateRequest.getPoolId())
              .orElseThrow(
                      () -> new BusinessException(BusinessCode.POOL_NOT_FOUND));

      ReportHistory reportHistory = this.initReportHistory(poolReportCreateRequest.getPoolId(), username);
      this.exportDirect(poolReportRepository.save(
              poolReportCreateRequest.toEntity(reportHistory, username)));
      return true;
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }

  @Override
  public PoolReportExportResponse export(Long reportId, ExportType exportType, String username) {
    try {
      if (!ExportType.CSV.equals(exportType) && !ExportType.EXCEL.equals(exportType)) {
        exportType = ExportType.CSV;
      }
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, reportId);
      String storageKey = null, reportName = null;
      if (poolReport.getReportHistory() != null) {
        storageKey = poolReport.getReportHistory().getStorageKey();
        reportName = poolReport.getReportHistory().getReportName();
      }

      if (DataUtil.isNullOrEmpty(storageKey)) {
        throw new BusinessException(BusinessCode.INTERNAL_ERROR);
      } else {
        byte[] bytes = storageService.downloadFile(storageKey + exportType.getValue());
        return PoolReportExportResponse.builder()
                .fileName(reportName + exportType.getValue())
                .byteArrayInputStream(new ByteArrayInputStream(bytes))
                .build();
      }
    } catch (BusinessException e) {
      throw e;
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  public void exportDirect(PoolReport poolReport) {
    try {
      if (poolReport.getReportHistory() == null) {
        poolReport.setReportHistory(this.initReportHistory(poolReport.getPoolView(), poolReport.getUsername()));
      }
      List<ExportContent> exportContents = new ArrayList<>();
      /// epoch size
      if (poolReport.getIsPoolSize()) {
        BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse = this.fetchEpochSize(poolReport);

        exportContents.add(ExportContent.builder()
                .clazz(PoolReportDetailResponse.EpochSize.class)
                .headerTitle(EPOCH_SIZE_TITLE)
                .lstColumn(PoolReportDetailResponse.EpochSize.designFile())
                .lstData(epochSizeBaseFilterResponse.getData())
                .build());
      }

      /// pool registrations
      if (poolReport.getIsRegistration()) {
        BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> poolRegistrationBaseFilterResponse = this.fetchPoolRegistration(
                poolReport);

        exportContents.add(ExportContent.builder()
                .clazz(PoolReportDetailResponse.PoolRegistration.class)
                .headerTitle(POOL_REGISTRATIONS_TITLE)
                .lstColumn(
                        PoolReportDetailResponse.PoolRegistration.designFile(poolReport.getIsFeesPaid()))
                .lstData(poolRegistrationBaseFilterResponse.getData())
                .build());
      }
      // pool update
      if (poolReport.getIsPoolUpdate()) {
        BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> poolUpdateBaseFilterResponse = this.fetchPoolUpdate(
                poolReport);

        exportContents.add(ExportContent.builder()
                .clazz(PoolReportDetailResponse.PoolUpdate.class)
                .headerTitle(POOL_UPDATE_TITLE)
                .lstColumn(PoolReportDetailResponse.PoolUpdate.designFile(poolReport.getIsFeesPaid()))
                .lstData(poolUpdateBaseFilterResponse.getData())
                .build());
      }
      // reward distribution
      if (poolReport.getIsReward()) {
        BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> rewardDistributionBaseFilterResponse = this.fetchRewardsDistribution(
                poolReport);

        exportContents.add(ExportContent.builder()
                .clazz(PoolReportDetailResponse.RewardDistribution.class)
                .headerTitle(REWARD_DISTRIBUTION_TITLE)
                .lstColumn(PoolReportDetailResponse.RewardDistribution.designFile())
                .lstData(rewardDistributionBaseFilterResponse.getData())
                .build());
      }
      // deregistration
      if (poolReport.getIsDeregistration()) {
        BaseFilterResponse<PoolReportDetailResponse.Deregistration> deregistrationBaseFilterResponse = this.fetchDeregistraion(
                poolReport);

        exportContents.add(ExportContent.builder()
                .clazz(PoolReportDetailResponse.Deregistration.class)
                .headerTitle(DEREGISTRATION_TITLE)
                .lstColumn(
                        PoolReportDetailResponse.Deregistration.designFile(poolReport.getIsFeesPaid()))
                .lstData(deregistrationBaseFilterResponse.getData())
                .build());
      }
      InputStream csvInputStream = CSVHelper.writeContent(exportContents);
      InputStream excelInputStream = ExcelHelper.writeContent(exportContents);
      String storageKey = generateStorageKey(poolReport);
      storageService.uploadFile(csvInputStream.readAllBytes(), storageKey + CSV_EXTENSION);
      storageService.uploadFile(excelInputStream.readAllBytes(), storageKey + EXCEL_EXTENSION);
      poolReport.getReportHistory().setStatus(ReportStatus.GENERATED);
      poolReport.getReportHistory().setStorageKey(storageKey);
      poolReportRepository.save(poolReport);
      excelInputStream.close();
      csvInputStream.close();
    } catch (IOException e) {
      log.error(e.getMessage(), e);
    } catch (RuntimeException e) {
      throw e;
    }

  }

  @Override
  public BaseFilterResponse<PoolReportListResponse> list(Pageable pageable, String username) {
    try {
      Page<PoolReport> poolReportPage = poolReportRepository.findByUsername(username, pageable);
      List<PoolReport> poolReports = poolReportPage.getContent();
      List<PoolReportListResponse> poolReportListResponses = poolReports.stream()
              .map(PoolReportListResponse::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(poolReportPage, poolReportListResponses);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  @Override
  public PoolReport detail(String reportId, String username) {
    try {
      return poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new PoolReport();
    }
  }

  @Override
  public PoolReportDetailResponse detailFull(String reportId, Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      PoolReportDetailResponse poolReportDetailResponse = new PoolReportDetailResponse();
      /// epoch size
      if (poolReport.getIsPoolSize()) {
        BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse = this.fetchEpochSize(
                reportId, pageable, username);
        poolReportDetailResponse.setEpochSizes(epochSizeBaseFilterResponse);
      }
      /// pool registrations
      if (poolReport.getIsRegistration()) {
        BaseFilterResponse<TabularRegisResponse> poolRegistrationBaseFilterResponse = this.fetchPoolRegistration(
                reportId, pageable, username);
        poolReportDetailResponse.setPoolRegistrations(poolRegistrationBaseFilterResponse);
      }
      // pool update
      if (poolReport.getIsPoolUpdate()) {
        BaseFilterResponse<PoolUpdateDetailResponse> poolUpdateBaseFilterResponse = this.fetchPoolUpdate(
                reportId, pageable, username);
        poolReportDetailResponse.setPoolUpdates(poolUpdateBaseFilterResponse);
      }
      // reward distribution
      if (poolReport.getIsReward()) {
        BaseFilterResponse<RewardResponse> rewardDistributionBaseFilterResponse = this.fetchRewardsDistribution(
                reportId, pageable, username);
        poolReportDetailResponse.setRewardDistributions(rewardDistributionBaseFilterResponse);
      }
      // deregistration
      if (poolReport.getIsDeregistration()) {
        BaseFilterResponse<DeRegistrationResponse> deregistrationBaseFilterResponse = this.fetchDeregistraion(
                reportId, pageable, username);
        poolReportDetailResponse.setDeregistrations(deregistrationBaseFilterResponse);
      }
      return poolReportDetailResponse;

    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new PoolReportDetailResponse();
    }
  }

  @Override
  public BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(String reportId,
                                                                               Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      Page<PoolReportProjection> epochSizeProjectionPage = epochStakeRepository.getEpochSizeByPoolReport(
              poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable);
      List<PoolReportProjection> epochSizeProjections = epochSizeProjectionPage.getContent();
      List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
              .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(epochSizeProjectionPage, epochSizes);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  @Override
  public BaseFilterResponse<TabularRegisResponse> fetchPoolRegistration(String reportId,
                                                                        Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      return poolLifecycleService.registrationList(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  @Override
  public BaseFilterResponse<PoolUpdateDetailResponse> fetchPoolUpdate(String reportId,
                                                                      Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      return poolLifecycleService.poolUpdateList(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  @Override
  public BaseFilterResponse<RewardResponse> fetchRewardsDistribution(String reportId,
                                                                     Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      return poolLifecycleService.listReward(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  @Override
  public BaseFilterResponse<DeRegistrationResponse> fetchDeregistraion(String reportId,
                                                                       Pageable pageable, String username) {
    try {
      PoolReport poolReport = poolReportRepository.findByUsernameAndId(username, Long.parseLong(reportId));
      return poolLifecycleService.deRegistration(poolReport.getPoolView(), null, null, null,
              pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  public BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(PoolReport poolReport) {
    List<PoolReportProjection> epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
            .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(epochSizes, epochSizeProjections.size());
  }

  public BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> fetchPoolRegistration(PoolReport poolReport) {
    try {
      List<TabularRegisResponse> tabularRegisResponses = poolLifecycleService.registrationList(poolReport.getPoolView(),
              PageRequest.of(0, Integer.MAX_VALUE, Sort.Direction.DESC, "id")).getData();
      List<PoolReportDetailResponse.PoolRegistration> poolRegistrations = tabularRegisResponses.stream().map(PoolReportDetailResponse.PoolRegistration::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(poolRegistrations, poolRegistrations.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  public BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> fetchPoolUpdate(PoolReport poolReport) {
    try {
      List<PoolUpdateDetailResponse> poolUpdateDetailResponses = poolLifecycleService.poolUpdateList(poolReport.getPoolView(),
              PageRequest.of(0, Integer.MAX_VALUE, Sort.Direction.DESC, "id")).getData();
      List<PoolReportDetailResponse.PoolUpdate> poolUpdates = poolUpdateDetailResponses.stream().map(PoolReportDetailResponse.PoolUpdate::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(poolUpdates, poolUpdates.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  public BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> fetchRewardsDistribution(PoolReport poolReport) {
    try {
      List<RewardResponse> rewardResponses = poolLifecycleService.listReward(poolReport.getPoolView(),
              PageRequest.of(0, Integer.MAX_VALUE, Sort.Direction.DESC, "id")).getData();
      List<PoolReportDetailResponse.RewardDistribution> rewardDistributions = rewardResponses.stream().map(PoolReportDetailResponse.RewardDistribution::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(rewardDistributions, rewardDistributions.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  public BaseFilterResponse<PoolReportDetailResponse.Deregistration> fetchDeregistraion(PoolReport poolReport) {
    try {
      List<DeRegistrationResponse> deRegistrationResponses = poolLifecycleService.deRegistration(poolReport.getPoolView(), null, null, null,
              PageRequest.of(0, Integer.MAX_VALUE, Sort.Direction.DESC, "id")).getData();
      List<PoolReportDetailResponse.Deregistration> deregistrations = deRegistrationResponses.stream().map(PoolReportDetailResponse.Deregistration::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(deregistrations, deregistrations.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return new BaseFilterResponse<>(new ArrayList<>(), 0);
    }
  }

  private String generateStorageKey(PoolReport PoolReport) {
    return PoolReport.getId() + "_" + PoolReport.getReportHistory().getReportName();
  }

  private ReportHistory initReportHistory(String poolId, String username) {
    return ReportHistory.builder()
            .reportName("report_pool_" + poolId)
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.POOL_ID)
            .username(username)
            .createdAt(new Timestamp(System.currentTimeMillis()))
            .build();
  }
}
