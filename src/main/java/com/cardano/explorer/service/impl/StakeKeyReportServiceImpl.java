package com.cardano.explorer.service.impl;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.StakeKeyReportMapper;
import com.cardano.explorer.model.request.report.ReportHistoryFilterRequest;
import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.report.ReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWalletActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.repository.ReportHistoryRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeKeyReportHistoryRepository;
import com.cardano.explorer.service.StakeKeyLifeCycleService;
import com.cardano.explorer.service.StakeKeyReportService;
import com.cardano.explorer.util.csv.CSVColumn;
import com.cardano.explorer.util.csv.CSVHelper;
import com.cardano.explorer.util.DataUtil;
import com.cardano.explorer.util.csv.ColumnFieldEnum;
import com.cardano.explorer.util.csv.ColumnTitleEnum;
import com.cardano.explorer.util.csv.ExportContent;
import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import com.sotatek.cardano.common.entity.StakingLifeCycleEvent;
import com.sotatek.cardano.common.enumeration.ReportStatus;
import com.sotatek.cardano.common.enumeration.ReportType;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional
public class StakeKeyReportServiceImpl implements StakeKeyReportService {

  public static final String DATE_TIME_PATTERN = "yyyyMMdd";
  public static final String ADA_TRANSFER_TITLE = "ADA Transfer";
  public static final String STAKE_KEY_LIFE_CYCLE_TITLE = "Stake Key Life Cycle";
  public static final String STAKE_KEY_REGISTRATION_TITLE = "Stake Key Registration";
  public static final String DELEGATION_HISTORY_TITLE = "Delegation History";
  public static final String REWARDS_DISTRIBUTION_TITLE = "Rewards Distribution";
  public static final String WITHDRAWAL_HISTORY_TITLE = "Withdrawal History";
  public static final String STAKE_KEY_DEREGISTRATION_TITLE = "Stake Key Deregistration";
  public static final String WALLET_ACTIVITY_TITLE = "Wallet Activity";
  public static final String REWARD_ACTIVITY_TITLE = "Reward Activity";
  public static final String MIN_TIME = "1970-01-01 00:00:00";
  private final Pageable defaultPageable = PageRequest.of(0, 1000);
  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;
  private final StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;
  private final ReportHistoryRepository reportHistoryRepository;
  private final StakeKeyReportMapper stakeKeyReportMapper;
  private final StakeAddressRepository stakeAddressRepository;
  private final AmazonS3 s3Client;
  @Value("${cloud.aws.s3.bucket.name}")
  private String bucketName;

  @Override
  public StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReport stakeKeyReport) {

    stakeAddressRepository.findByView(stakeKeyReport.getStakeKey())
        .orElseThrow(
            () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportMapper.toStakeKeyReportHistory(
        stakeKeyReport);

    if (DataUtil.isNullOrEmpty(stakeKeyReport.getReportName())) {
      String reportName = generateReportName(stakeKeyReportHistory);
      stakeKeyReportHistory.getReportHistory().setReportName(reportName);
    }

    stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.IN_PROGRESS);
    stakeKeyReportHistory.getReportHistory().setType(ReportType.STAKE_KEY);

    stakeKeyReportHistory = stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);
    return stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory);
  }

  private StakeKeyReportResponse exportStakeKeyReport(StakeKeyReportHistory stakeKeyReportHistory) {
    try {
      List<StakingLifeCycleEvent> stakingLifeCycleEvents = stakeKeyReportHistory.getStakingLifeCycleEvents()
          .stream()
          .sorted(Comparator.comparingInt(o -> o.getType().getValue()))
          .collect(Collectors.toList());
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = new StakeLifeCycleFilterRequest();
      stakeLifeCycleFilterRequest.setFromDate(stakeKeyReportHistory.getFromDate());
      stakeLifeCycleFilterRequest.setToDate(stakeKeyReportHistory.getToDate());

      List<ExportContent> exportContents = new ArrayList<>();

      if (Boolean.TRUE.equals(stakeKeyReportHistory.getIsADATransfer())) {
        exportContents.add(ExportContent.builder()
            .headerTitle(ADA_TRANSFER_TITLE)
            .build());

        exportContents.add(exportStakeWalletActivitys(stakeKeyReportHistory.getStakeKey()));
        exportContents.add(exportStakeRewardActivitys(stakeKeyReportHistory.getStakeKey()));

        if (Boolean.TRUE.equals(stakeKeyReportHistory.getIsFeesPaid())) {
          // TODO: export fees paid
        }
      }

      exportContents.add(ExportContent.builder()
          .headerTitle(STAKE_KEY_LIFE_CYCLE_TITLE)
          .build());

      for (int i = 0; i < stakingLifeCycleEvents.size(); i++) {
        StakingLifeCycleEvent stakingLifeCycleEvent = stakingLifeCycleEvents.get(i);
        ExportContent exportContent = ExportContent.builder().build();
        switch (stakingLifeCycleEvent.getType()) {
          case REGISTRATION:
            exportContent = exportStakeRegistrations(stakeKeyReportHistory.getStakeKey(),
                stakeLifeCycleFilterRequest);
            break;
          case DELEGATION:
            exportContent = exportStakeDelegations(stakeKeyReportHistory.getStakeKey(),
                stakeLifeCycleFilterRequest);
            break;
          case REWARDS:
            exportContent = exportStakeRewards(stakeKeyReportHistory.getStakeKey());
            break;
          case WITHDRAWAL:
            exportContent = exportStakeWithdrawals(stakeKeyReportHistory.getStakeKey(),
                stakeLifeCycleFilterRequest);
            break;
          case DEREGISTRATION:
            exportContent = exportStakeDeregistrations(stakeKeyReportHistory.getStakeKey(),
                stakeLifeCycleFilterRequest);
            break;
        }
        exportContents.add(exportContent);
      }

      InputStream inputStream = CSVHelper.writeContent(exportContents);
      byte[] bytes = inputStream.readAllBytes();
      String storageKey = generateStorageKey(stakeKeyReportHistory);
      uploadFile(bytes, storageKey);
      stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.GENERATED);
      stakeKeyReportHistory.getReportHistory().setStorageKey(storageKey);
      stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);
      inputStream.close();
      return StakeKeyReportResponse.builder()
          .fileName(stakeKeyReportHistory.getReportHistory().getReportName())
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(
      Pageable pageable) {
    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse = stakeKeyReportHistoryRepository.findAll(
            pageable)
        .map(stakeKeyReportMapper::toStakeKeyReportHistoryResponse);

    return new BaseFilterResponse<>(stakeKeyReportHistoriesResponse);
  }

  @Override
  public BaseFilterResponse<ReportHistoryResponse> getReportHistory(
      ReportHistoryFilterRequest filterRequest, Pageable pageable) {
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
            reportName, fromDate, toDate, pageable)
        .map(reportHistoryProjection -> ReportHistoryResponse.builder()
            .id(reportHistoryProjection.getId())
            .reportName(reportHistoryProjection.getReportName())
            .status(reportHistoryProjection.getStatus())
            .type(reportHistoryProjection.getType())
            .createdAt(reportHistoryProjection.getCreatedAt())
            .build());

    return new BaseFilterResponse<>(reportHistoryProjections);
  }

  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistoryByStakeKey(
      String stakeKey, Pageable pageable) {

    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse = stakeKeyReportHistoryRepository.findByStakeKey(
            stakeKey, pageable)
        .map(stakeKeyReportMapper::toStakeKeyReportHistoryResponse);
    return new BaseFilterResponse<>(stakeKeyReportHistoriesResponse);
  }


  @Override
  public StakeKeyReportResponse exportStakeKeyReport(Long reportId) {
    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportHistoryRepository.findById(reportId)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_REPORT_HISTORY_NOT_FOUND));
    String storageKey = stakeKeyReportHistory.getReportHistory().getStorageKey();
    String reportName = stakeKeyReportHistory.getReportHistory().getReportName();
    if (DataUtil.isNullOrEmpty(storageKey)) {
      return exportStakeKeyReport(stakeKeyReportHistory);
    } else {
      byte[] bytes = downloadFile(storageKey);
      return StakeKeyReportResponse.builder()
          .fileName(reportName)
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    }
  }

  @Override
  public void uploadFile(byte[] bytes, String fileName) {
    ObjectMetadata metadata = new ObjectMetadata();
    metadata.setContentType("text/csv");
    metadata.setContentLength(bytes.length);
    s3Client.putObject(
        new PutObjectRequest(bucketName, fileName, new ByteArrayInputStream(bytes), metadata));
  }

  @Override
  public byte[] downloadFile(String fileName) {
    S3Object s3Object = s3Client.getObject(bucketName, fileName);
    S3ObjectInputStream inputStream = s3Object.getObjectContent();
    try {
      return inputStream.readAllBytes();
    } catch (IOException e) {
      e.printStackTrace();
      return new byte[0];
    }
  }

  private ExportContent exportStakeWalletActivitys(String stakeKey) {
    List<StakeWalletActivityResponse> stakeWalletActivitys = stakeKeyLifeCycleService.getStakeWalletActivities(
        stakeKey, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeWalletActivityResponse.class)
        .headerTitle(WALLET_ACTIVITY_TITLE)
        .lstColumn(buildStakeWalletActivityColumn())
        .lstData(stakeWalletActivitys)
        .build();
  }

  private ExportContent exportStakeRewardActivitys(String stakeKey) {
    List<StakeRewardActivityResponse> stakeRewardActivitys = stakeKeyLifeCycleService.getStakeRewardActivities(
        stakeKey, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeRewardActivityResponse.class)
        .headerTitle(REWARD_ACTIVITY_TITLE)
        .lstColumn(buildStakeRewardActivityColumn())
        .lstData(stakeRewardActivitys)
        .build();
  }

  private ExportContent exportStakeRegistrations(String stakeKey,
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeRegistrationLifeCycle> stakeRegistrations = stakeKeyLifeCycleService.getStakeRegistrations(
        stakeKey, stakeLifeCycleFilterRequest, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeRegistrationLifeCycle.class)
        .headerTitle(STAKE_KEY_REGISTRATION_TITLE)
        .lstColumn(buildStakeRegistrationColumn())
        .lstData(stakeRegistrations)
        .build();
  }

  private ExportContent exportStakeDelegations(String stakeKey,
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeDelegationFilterResponse> stakeDelegations = stakeKeyLifeCycleService.getStakeDelegations(
        stakeKey, stakeLifeCycleFilterRequest, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeDelegationFilterResponse.class)
        .headerTitle(DELEGATION_HISTORY_TITLE)
        .lstColumn(buildStakeDelegationColumn())
        .lstData(stakeDelegations)
        .build();
  }

  private ExportContent exportStakeRewards(String stakeKey) {
    List<StakeRewardResponse> stakeRewards = stakeKeyLifeCycleService.getStakeRewards(
        stakeKey, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeRewardResponse.class)
        .headerTitle(REWARDS_DISTRIBUTION_TITLE)
        .lstColumn(buildStakeRewardColumn())
        .lstData(stakeRewards)
        .build();
  }

  private ExportContent exportStakeWithdrawals(String stakeKey,
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeWithdrawalFilterResponse> stakeWithdrawals = stakeKeyLifeCycleService.getStakeWithdrawals(
        stakeKey, stakeLifeCycleFilterRequest, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeWithdrawalFilterResponse.class)
        .headerTitle(WITHDRAWAL_HISTORY_TITLE)
        .lstColumn(buildStakeWithdrawalColumn())
        .lstData(stakeWithdrawals)
        .build();
  }

  private ExportContent exportStakeDeregistrations(String stakeKey,
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeRegistrationLifeCycle> stakeDeRegistrations = stakeKeyLifeCycleService.getStakeDeRegistrations(
        stakeKey, stakeLifeCycleFilterRequest, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeRegistrationLifeCycle.class)
        .headerTitle(STAKE_KEY_DEREGISTRATION_TITLE)
        .lstColumn(buildStakeDeRegistrationColumn())
        .lstData(stakeDeRegistrations)
        .build();
  }

  private List<CSVColumn> buildStakeRegistrationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.DEPOSIT_COLUMN, ColumnTitleEnum.DEPOSIT_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.FEE_COLUMN, ColumnTitleEnum.FEES_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeDelegationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.OUT_SUM_COLUMN, ColumnTitleEnum.FEES_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeRewardColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.EPOCH_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.AMOUNT_COLUMN, ColumnTitleEnum.REWARDS_PAID_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeWithdrawalColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.NET_AMOUNT_COLUMN, ColumnTitleEnum.AMOUNT_NET_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeDeRegistrationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.DEPOSIT_COLUMN, ColumnTitleEnum.DEPOSIT_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.FEE_COLUMN, ColumnTitleEnum.FEES_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeWalletActivityColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.TX_HASH_COLUMN, ColumnTitleEnum.TX_HASH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.AMOUNT_COLUMN, ColumnTitleEnum.AMOUNT_ADA_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TYPE_COLUMN, ColumnTitleEnum.TX_TYPE_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.STATUS_COLUMN, ColumnTitleEnum.STATUS_TITLE));
    return columns;
  }

  private List<CSVColumn> buildStakeRewardActivityColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn(ColumnFieldEnum.EPOCH_NO_COLUMN, ColumnTitleEnum.EPOCH_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TIME_COLUMN, ColumnTitleEnum.TIMESTAMP_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.AMOUNT_COLUMN, ColumnTitleEnum.AMOUNT_ADA_TITLE));
    columns.add(new CSVColumn(ColumnFieldEnum.TYPE_COLUMN, ColumnTitleEnum.TX_TYPE_TITLE));
    return columns;
  }

  private String instanceToString(Instant instant) {
    return DataUtil.instantToString(instant, DATE_TIME_PATTERN);
  }

  private String generateReportName(StakeKeyReportHistory stakeKeyReportHistory) {
    return "report_stake_" + stakeKeyReportHistory.getStakeKey() + "_" +
        instanceToString(stakeKeyReportHistory.getFromDate().toInstant()) + "_" + instanceToString(
        stakeKeyReportHistory.getToDate().toInstant());
  }

  private String generateStorageKey(StakeKeyReportHistory stakeKeyReportHistory) {
    return stakeKeyReportHistory.getReportHistory().getId() + "_"
        + stakeKeyReportHistory.getReportHistory()
        .getReportName();
  }
}
