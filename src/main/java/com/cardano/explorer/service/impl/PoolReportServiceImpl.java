package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.PoolReportEvent;
import com.cardano.explorer.model.request.pool.report.PoolReportCreateRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.pool.projection.PoolReportProjection;
import com.cardano.explorer.model.response.pool.report.PoolReportDetailResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportExportResponse;
import com.cardano.explorer.model.response.pool.report.PoolReportListResponse;
import com.cardano.explorer.repository.EpochStakeRepository;
import com.cardano.explorer.repository.PoolReportRepository;
import com.cardano.explorer.repository.PoolRetireRepository;
import com.cardano.explorer.repository.PoolUpdateRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.service.PoolReportService;
import com.cardano.explorer.service.StakeKeyReportService;
import com.cardano.explorer.util.DataUtil;
import com.cardano.explorer.util.csv.CSVHelper;
import com.sotatek.cardano.common.entity.PoolReport;
import com.sotatek.cardano.common.entity.ReportHistory;
import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
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

  @Override
  public Boolean create(PoolReportCreateRequest poolReportCreateRequest) {
    try {
      //TODO check not null for fields
      //TODO check if duplicate report?
      ReportHistory reportHistory = ReportHistory.builder()
          .reportName("report_pool_" + poolReportCreateRequest.getPoolId())
          .status(ReportStatus.IN_PROGRESS)
          .type(ReportType.STAKE_KEY)
          .username(poolReportCreateRequest.getPoolId())
          .createdAt(new Timestamp(System.currentTimeMillis()))
          .build();
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
  public PoolReportDetailResponse detail(String reportId, Pageable pageable) {
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
      String storageKey = poolReport.getReportHistory().getStorageKey();
      String reportName = poolReport.getReportHistory().getReportName();
      if (DataUtil.isNullOrEmpty(storageKey)) {
        InputStream inputStream = InputStream.nullInputStream();
        /// epoch size
        if (poolReport.getIsPoolSize()) {
          BaseFilterResponse<PoolReportDetailResponse.EpochSize> epochSizeBaseFilterResponse = this.fetchEpochSize(
              poolReport, null);
          inputStream = CSVHelper.writeContent(epochSizeBaseFilterResponse.getData(),
              PoolReportDetailResponse.EpochSize.designFile(), inputStream.readAllBytes(),
              EPOCH_SIZE_TITLE);
        }
        /// all
        boolean isAll = poolReport.getEvent().contains(PoolReportEvent.ALL.getValue());
        inputStream = CSVHelper.writeContent(inputStream.readAllBytes(), LIFE_CYCLE_TITLE);
        /// pool registrations
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.REGISTRATION.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.PoolRegistration> poolRegistrationBaseFilterResponse = this.fetchPoolRegistrations(
              poolReport);
          inputStream = CSVHelper.writeContent(poolRegistrationBaseFilterResponse.getData(),
              PoolReportDetailResponse.PoolRegistration.designFile(poolReport.getIsFeesPaid()),
              inputStream.readAllBytes(), POOL_REGISTRATIONS_TITLE);
        }
        // pool update
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.POOL_UPDATE.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.PoolUpdate> poolUpdateBaseFilterResponse = this.fetchPoolUpdate(
              poolReport);
          inputStream = CSVHelper.writeContent(poolUpdateBaseFilterResponse.getData(),
              PoolReportDetailResponse.PoolUpdate.designFile(poolReport.getIsFeesPaid()),
              inputStream.readAllBytes(), POOL_UPDATE_TITLE);
        }
        // reward distribution
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.REWARD.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.RewardDistribution> rewardDistributionBaseFilterResponse = this.fetchRewardDistribution(
              poolReport);
          inputStream = CSVHelper.writeContent(rewardDistributionBaseFilterResponse.getData(),
              PoolReportDetailResponse.RewardDistribution.designFile(), inputStream.readAllBytes(),
              REWARD_DISTRIBUTION_TITLE);
        }
        // deregistration
        if (isAll || poolReport.getEvent().contains(PoolReportEvent.DEREGISTRATION.getValue())) {
          BaseFilterResponse<PoolReportDetailResponse.Deregistration> deregistrationBaseFilterResponse = this.fetchDeregistration(
              poolReport);
          inputStream = CSVHelper.writeContent(deregistrationBaseFilterResponse.getData(),
              PoolReportDetailResponse.Deregistration.designFile(poolReport.getIsFeesPaid()),
              inputStream.readAllBytes(), DEREGISTRATION_TITLE);
        }

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
}
