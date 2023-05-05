package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.lifecycle.DeRegistrationResponse;
import com.cardano.explorer.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import com.cardano.explorer.model.response.pool.lifecycle.RewardResponse;
import com.cardano.explorer.model.response.pool.lifecycle.TabularRegisResponse;
import com.cardano.explorer.model.response.pool.projection.PoolReportProjection;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportExportResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolReportRepository;
import com.cardano.explorer.repository.PoolRetireRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.service.PoolLifecycleService;
import com.cardano.explorer.service.PoolReportService;
import com.cardano.explorer.service.StakeKeyReportService;
import com.cardano.explorer.util.DataUtil;
import com.cardano.explorer.util.report.CSVHelper;
import com.cardano.explorer.util.report.ExportContent;
import com.sotatek.cardano.common.entity.PoolReport;
import com.sotatek.cardano.common.entity.ReportHistory;
import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class PoolReportServiceImpl implements PoolReportService {

  public static final String EPOCH_SIZE_TITLE = "Epoch Size";

  public static final String POOL_REGISTRATIONS_TITLE = "Pool Registrations";

  public static final String POOL_UPDATE_TITLE = "Pool Update";

  public static final String REWARD_DISTRIBUTION_TITLE = "Reward Distribution";

  public static final String DEREGISTRATION_TITLE = "Deregistration";

  public static final String LIFE_CYCLE_TITLE = "Pool Report Life Cycle";

  private final PoolReportRepository poolReportRepository;

  private final EpochStakeRepository epochStakeRepository;

  private final PoolUpdateRepository poolUpdateRepository;

  private final RewardRepository rewardRepository;

  private final PoolRetireRepository poolRetireRepository;

  private final StakeKeyReportService stakeKeyReportService;

  private final PoolLifecycleService poolLifecycleService;

  @Override
  public Boolean create(PoolReportCreateRequest poolReportCreateRequest) {
    try {
      //TODO check not null for fields
      //TODO check if duplicate report?
      ReportHistory reportHistory = this.initReportHistory(poolReportCreateRequest.getPoolId());
      poolReportRepository.save(poolReportCreateRequest.toEntity(reportHistory));
      return true;
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return false;
    }
  }

  @Override
  public BaseFilterResponse<PoolReportListResponse> list(Pageable pageable) {
    try {
      //FIXME replace: find by username
      List<PoolReport> poolReports = poolReportRepository.findAll(pageable).getContent();
      List<PoolReportListResponse> poolReportListResponses = poolReports.stream()
          .map(PoolReportListResponse::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(poolReportListResponses, poolReportListResponses.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public PoolReportDetailResponse detailFull(String reportId, Pageable pageable) {
    try {
      //FIXME replace: find by username
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      PoolReportDetailResponse poolReportDetailResponse = new PoolReportDetailResponse();
      /// epoch size
      if (poolReport.getIsPoolSize()) {
        BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse = this.fetchEpochSize(
            poolReport, pageable);
        poolReportDetailResponse.setEpochSizes(epochSizeBaseFilterResponse);
      }
      /// all
      boolean isAll = poolReport.getEvent().contains(PoolReportEvent.ALL.getValue());
      /// pool registrations
      if (isAll || poolReport.getEvent().contains(PoolReportEvent.REGISTRATION.getValue())) {
        BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> poolRegistrationBaseFilterResponse = this.fetchPoolRegistrations(
            poolReport);
        poolReportDetailResponse.setPoolRegistrations(poolRegistrationBaseFilterResponse);
      }
      // pool update
      if (isAll || poolReport.getEvent().contains(PoolReportEvent.POOL_UPDATE.getValue())) {
        BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> poolUpdateBaseFilterResponse = this.fetchPoolUpdate(
            poolReport);
        poolReportDetailResponse.setPoolUpdates(poolUpdateBaseFilterResponse);
      }
      // reward distribution
      if (isAll || poolReport.getEvent().contains(PoolReportEvent.REWARD.getValue())) {
        BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> rewardDistributionBaseFilterResponse = this.fetchRewardDistribution(
            poolReport);
        poolReportDetailResponse.setRewardDistributions(rewardDistributionBaseFilterResponse);
      }
      // deregistration
      if (isAll || poolReport.getEvent().contains(PoolReportEvent.DEREGISTRATION.getValue())) {
        BaseFilterResponse<PoolReportDetailResponse.Deregistration> deregistrationBaseFilterResponse = this.fetchDeregistration(
            poolReport);
        poolReportDetailResponse.setDeregistrations(deregistrationBaseFilterResponse);
      }
      return poolReportDetailResponse;

    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public BaseFilterResponse<PoolReportDetailResponse.EpochSize> detailEpochSize(String reportId,
      Pageable pageable) {
    try {
      //FIXME replace: find by username
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      List<PoolReportProjection> epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(
              poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable)
          .getContent();
      List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
          .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
      return new BaseFilterResponse<>(epochSizes, epochSizes.size());
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public PoolReportExportResponse export(Long reportId) {
    try {
      PoolReport poolReport = poolReportRepository.findById(reportId).get();
      String storageKey = null, reportName = null;
      if (poolReport.getReportHistory() != null) {
        storageKey = poolReport.getReportHistory().getStorageKey();
        reportName = poolReport.getReportHistory().getReportName();
      } else {
        poolReport.setReportHistory(this.initReportHistory(poolReport.getPoolView()));
      }

      if (DataUtil.isNullOrEmpty(storageKey)) {
        List<ExportContent> exportContents = new ArrayList<>();
        /// epoch size
        if (poolReport.getIsPoolSize()) {
          BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse = this.fetchEpochSize(
              poolReport, null);

          exportContents.add(ExportContent.builder()
              .clazz(PoolReportDetailResponse.EpochSize.class)
              .headerTitle(EPOCH_SIZE_TITLE)
              .lstColumn(PoolReportDetailResponse.EpochSize.designFile())
              .lstData(epochSizeBaseFilterResponse.getData())
              .build());
        }
        /// all
        boolean isAll = poolReport.getEvent().contains(PoolReportEvent.ALL.getValue());
        exportContents.add(ExportContent.builder()
            .headerTitle(LIFE_CYCLE_TITLE)
            .build());
        /// pool registrations
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.REGISTRATION.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> poolRegistrationBaseFilterResponse = this.fetchPoolRegistrations(
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
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.POOL_UPDATE.getValue())) {
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
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.REWARD.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> rewardDistributionBaseFilterResponse = this.fetchRewardDistribution(
              poolReport);

          exportContents.add(ExportContent.builder()
              .clazz(PoolReportDetailResponse.RewardDistribution.class)
              .headerTitle(REWARD_DISTRIBUTION_TITLE)
              .lstColumn(PoolReportDetailResponse.RewardDistribution.designFile())
              .lstData(rewardDistributionBaseFilterResponse.getData())
              .build());
        }
        // deregistration
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.DEREGISTRATION.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.Deregistration> deregistrationBaseFilterResponse = this.fetchDeregistration(
              poolReport);

          exportContents.add(ExportContent.builder()
              .clazz(PoolReportDetailResponse.Deregistration.class)
              .headerTitle(DEREGISTRATION_TITLE)
              .lstColumn(
                  PoolReportDetailResponse.Deregistration.designFile(poolReport.getIsFeesPaid()))
              .lstData(deregistrationBaseFilterResponse.getData())
              .build());
        }

        InputStream inputStream = CSVHelper.writeContent(exportContents);
        byte[] bytes = inputStream.readAllBytes();
        storageKey = generateStorageKey(poolReport);
        stakeKeyReportService.uploadFile(bytes, storageKey);
        poolReport.getReportHistory().setStatus(ReportStatus.GENERATED);
        poolReport.getReportHistory().setStorageKey(storageKey);
        poolReportRepository.save(poolReport);

        inputStream.close();
        return PoolReportExportResponse.builder()
            .fileName(poolReport.getReportHistory().getReportName())
            .byteArrayInputStream(new ByteArrayInputStream(bytes))
            .build();
      } else {
        byte[] bytes = stakeKeyReportService.downloadFile(storageKey);
        return PoolReportExportResponse.builder()
            .fileName(reportName)
            .byteArrayInputStream(new ByteArrayInputStream(bytes))
            .build();
      }
    } catch (IOException e) {
      log.error(e.getMessage(), e);
      return null;
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public BaseFilterResponse<TabularRegisResponse> detailPoolRegistration(String reportId,
      Pageable pageable) {
    try {
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      return poolLifecycleService.registrationList(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public BaseFilterResponse<PoolUpdateDetailResponse> detailPoolUpdate(String reportId,
      Pageable pageable) {
    try {
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      return poolLifecycleService.poolUpdateList(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public BaseFilterResponse<RewardResponse> detailRewardsDistribution(String reportId,
      Pageable pageable) {
    try {
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      return poolLifecycleService.listReward(poolReport.getPoolView(), pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public BaseFilterResponse<DeRegistrationResponse> detailDeregistraion(String reportId,
      Pageable pageable) {
    try {
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      return poolLifecycleService.deRegistration(poolReport.getPoolView(), null, null, null,
          pageable);
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  @Override
  public PoolReport detail(String reportId) {
    try {
      PoolReport poolReport = poolReportRepository.findById(Long.parseLong(reportId)).get();
      return poolReport;
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      return null;
    }
  }

  private BaseFilterResponse<PoolReportDetailResponse.EpochSize> fetchEpochSize(
      PoolReport poolReport, Pageable pageable) {
    List<PoolReportProjection> epochSizeProjections = null;
    if (pageable == null) {
      epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(poolReport.getPoolView(),
          poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    } else {
      epochSizeProjections = epochStakeRepository.getEpochSizeByPoolReport(poolReport.getPoolView(),
          poolReport.getBeginEpoch(), poolReport.getEndEpoch(), pageable).getContent();
    }
    List<PoolReportDetailResponse.EpochSize> epochSizes = epochSizeProjections.stream()
        .map(PoolReportDetailResponse.EpochSize::toDomain).collect(Collectors.toList());
    BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse =
        new BaseFilterResponse<>(epochSizes, epochSizes.size());
    return new BaseFilterResponse<>(epochSizes, epochSizes.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> fetchPoolRegistrations(
      PoolReport poolReport) {
    List<PoolReportProjection> poolRegistrationsProjections = poolUpdateRepository.getPoolRegistrationByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.PoolRegistration> poolRegistrations = poolRegistrationsProjections.stream()
        .map(PoolReportDetailResponse.PoolRegistration::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(poolRegistrations, poolRegistrations.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> fetchPoolUpdate(
      PoolReport poolReport) {
    List<PoolReportProjection> poolRegistrationsProjections = poolUpdateRepository.getPoolRegistrationByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.PoolUpdate> poolUpdates = poolRegistrationsProjections.stream()
        .map(PoolReportDetailResponse.PoolUpdate::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(poolUpdates, poolUpdates.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> fetchRewardDistribution(
      PoolReport poolReport) {
    List<PoolReportProjection> rewardDistributionProjections = rewardRepository.getRewardDistributionByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.RewardDistribution> rewardDistributions = rewardDistributionProjections.stream()
        .map(PoolReportDetailResponse.RewardDistribution::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(rewardDistributions, rewardDistributions.size());
  }

  private BaseFilterResponse<PoolReportDetailResponse.Deregistration> fetchDeregistration(
      PoolReport poolReport) {
    List<PoolReportProjection> deregistrationProjections = poolRetireRepository.getDeregistrationByPoolReport(
        poolReport.getPoolView(), poolReport.getBeginEpoch(), poolReport.getEndEpoch());
    List<PoolReportDetailResponse.Deregistration> deregistrations = deregistrationProjections.stream()
        .map(PoolReportDetailResponse.Deregistration::toDomain).collect(Collectors.toList());
    return new BaseFilterResponse<>(deregistrations, deregistrations.size());
  }

  private String generateStorageKey(PoolReport PoolReport) {
    return PoolReport.getId() + "_" + PoolReport.getReportHistory().getReportName();
  }

  private ReportHistory initReportHistory(String poolId) {
    return ReportHistory.builder()
        .reportName("report_pool_" + poolId)
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.POOL_ID)
        .username(poolId)
        .createdAt(new Timestamp(System.currentTimeMillis()))
        .build();
  }
}
