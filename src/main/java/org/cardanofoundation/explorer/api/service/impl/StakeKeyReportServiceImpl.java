package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.StakeKeyReportMapper;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.StakeKeyReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.StakeKeyLifeCycleService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import org.cardanofoundation.explorer.api.service.StorageService;
import org.cardanofoundation.explorer.api.util.report.ExcelHelper;
import org.cardanofoundation.explorer.api.util.report.CSVHelper;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.report.ExportContent;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.math.BigInteger;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyReportServiceImpl implements StakeKeyReportService {

  public static final String DATE_TIME_PATTERN = "yyyyMMdd";
  public static final String STAKE_KEY_REGISTRATION_TITLE = "Stake Key Registration";
  public static final String DELEGATION_HISTORY_TITLE = "Delegation History";
  public static final String REWARDS_DISTRIBUTION_TITLE = "Rewards Distribution";
  public static final String WITHDRAWAL_HISTORY_TITLE = "Withdrawal History";
  public static final String STAKE_KEY_DEREGISTRATION_TITLE = "Stake Key Deregistration";
  public static final String WALLET_ACTIVITY_TITLE = "Wallet Activity";
  public static final String CSV_EXTENSION = ".csv";
  public static final String EXCEL_EXTENSION = ".xlsx";
  private final Pageable defaultPageable = PageRequest.of(0, 1000, Sort.by("time").descending());
  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;
  private final StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;
  private final StakeKeyReportMapper stakeKeyReportMapper;
  private final StakeAddressRepository stakeAddressRepository;
  private final StorageService storageService;

  @Override
  @Transactional
  public StakeKeyReportHistoryResponse generateStakeKeyReport(
      StakeKeyReportRequest stakeKeyReportRequest, String username) {

    stakeAddressRepository.findByView(stakeKeyReportRequest.getStakeKey())
        .orElseThrow(
            () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportMapper.toStakeKeyReportHistory(
        stakeKeyReportRequest);

    if (DataUtil.isNullOrEmpty(stakeKeyReportRequest.getReportName())) {
      String reportName = generateReportName(stakeKeyReportHistory);
      stakeKeyReportHistory.getReportHistory().setReportName(reportName);
    }

    stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.IN_PROGRESS);
    stakeKeyReportHistory.getReportHistory().setType(ReportType.STAKE_KEY);
    stakeKeyReportHistory.getReportHistory().setUsername(username);

    stakeKeyReportHistory = stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);

    return stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory);
  }

  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(String username,
                                                                                    Pageable pageable) {
    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse =
        stakeKeyReportHistoryRepository.findByUsername(username, pageable)
            .map(stakeKeyReportMapper::toStakeKeyReportHistoryResponse);

    return new BaseFilterResponse<>(stakeKeyReportHistoriesResponse);
  }

  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistoryByStakeKey(
      String stakeKey, String username, Pageable pageable) {

    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse = stakeKeyReportHistoryRepository.findByUsernameAndStakeKey(
            stakeKey, username, pageable)
        .map(stakeKeyReportMapper::toStakeKeyReportHistoryResponse);
    return new BaseFilterResponse<>(stakeKeyReportHistoriesResponse);
  }

  @Override
  public StakeKeyReportResponse exportStakeKeyReport(Long reportId, String username,
                                                     ExportType exportType) {
    if (!ExportType.CSV.equals(exportType) && !ExportType.EXCEL.equals(exportType)) {
      exportType = ExportType.CSV;
    }

    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    String storageKey = stakeKeyReportHistory.getReportHistory().getStorageKey();
    String reportName = stakeKeyReportHistory.getReportHistory().getReportName();
    ReportStatus reportStatus = stakeKeyReportHistory.getReportHistory().getStatus();
    if (DataUtil.isNullOrEmpty(storageKey) || ReportStatus.IN_PROGRESS.equals(reportStatus)) {
      throw new BusinessException(BusinessCode.REPORT_IS_IN_PROGRESS);
    } else {
      byte[] bytes = storageService.downloadFile(storageKey + exportType.getValue());
      return StakeKeyReportResponse.builder()
          .fileName(reportName + exportType.getValue())
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    }
  }

  @Override
  public StakeKeyReportHistoryResponse getStakeKeyReportHistoryByReportId(Long reportId,
                                                                          String username) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);

    return stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory);
  }

  private StakeKeyReportHistory getStakeKeyReportHistory(Long reportId, String username) {
    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportHistoryRepository.findById(reportId)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_REPORT_HISTORY_NOT_FOUND));

    if (DataUtil.isNullOrEmpty(username) || !username.equals(
        stakeKeyReportHistory.getReportHistory().getUsername())) {
      throw new BusinessException(BusinessCode.STAKE_REPORT_HISTORY_NOT_FOUND);
    }
    return stakeKeyReportHistory;
  }

  @Override
  public BaseFilterResponse<StakeRegistrationLifeCycle> getStakeRegistrationsByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    return stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, stakeLifeCycleFilterRequest,
                                                          pageable);
  }

  @Override
  public BaseFilterResponse<StakeRegistrationLifeCycle> getStakeDeRegistrationsByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    return stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, stakeLifeCycleFilterRequest,
                                                            pageable);
  }

  @Override
  public BaseFilterResponse<StakeDelegationFilterResponse> getStakeDelegationsByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    return stakeKeyLifeCycleService.getStakeDelegations(stakeKey, stakeLifeCycleFilterRequest,
                                                        pageable);
  }

  @Override
  public BaseFilterResponse<StakeRewardResponse> getStakeRewardsByReportId(Long reportId,
                                                                           String username,
                                                                           Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);

    List<StakeRewardResponse> stakeRewardResponses = stakeKeyLifeCycleService
        .getStakeRewards(stakeKey, pageable).getData()
        .stream()
        .filter(response -> isTimeInFilterRange(response.getTime(), stakeLifeCycleFilterRequest))
        .toList();

    return new BaseFilterResponse<>(
        new PageImpl<>(stakeRewardResponses, pageable, stakeRewardResponses.size()));
  }

  @Override
  public BaseFilterResponse<StakeWithdrawalFilterResponse> getStakeWithdrawalsByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    return stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, stakeLifeCycleFilterRequest,
                                                        pageable);
  }

  @Override
  public BaseFilterResponse<StakeWalletActivityResponse> getWalletActivitiesByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);
    List<StakeWalletActivityResponse> stakeWalletActivityResponses = stakeKeyLifeCycleService
        .getStakeWalletActivities(stakeKey, pageable).getData()
        .stream()
        .filter(
            response -> isTimeInFilterRange(Date.from(response.getTime().toInstant(ZoneOffset.UTC)),
                                            stakeLifeCycleFilterRequest))
        .toList();

    return new BaseFilterResponse<>(new PageImpl<>(stakeWalletActivityResponses, pageable,
                                                   stakeWalletActivityResponses.size()));
  }

  @Override
  public BaseFilterResponse<StakeRewardActivityResponse> getRewardActivitiesByReportId(
      Long reportId, String username, Pageable pageable) {
    StakeKeyReportHistory stakeKeyReportHistory = getStakeKeyReportHistory(reportId, username);
    String stakeKey = stakeKeyReportHistory.getStakeKey();
    return stakeKeyLifeCycleService.getStakeRewardActivities(stakeKey, pageable);
  }

  @Transactional
  public void exportStakeKeyReport(StakeKeyReportHistory stakeKeyReportHistory) {
    try {
      List<ExportContent> exportContents = getExportContents(stakeKeyReportHistory);
      String storageKey = generateStorageKey(stakeKeyReportHistory);
      String csvFileName = storageKey + CSV_EXTENSION;
      String excelFileName = storageKey + EXCEL_EXTENSION;
      InputStream csvInputStream = CSVHelper.writeContent(exportContents);
      InputStream excelInputStream = ExcelHelper.writeContent(exportContents);
      storageService.uploadFile(csvInputStream.readAllBytes(), csvFileName);
      storageService.uploadFile(excelInputStream.readAllBytes(), excelFileName);
      stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.GENERATED);
      stakeKeyReportHistory.getReportHistory().setStorageKey(storageKey);
    } catch (Exception e) {
      stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.FAILED);
      e.printStackTrace();
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    } finally {
      stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);
    }
  }

  private List<ExportContent> getExportContents(StakeKeyReportHistory stakeKeyReportHistory) {
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);

    List<ExportContent> exportContents = new ArrayList<>();

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getIsADATransfer())) {
      exportContents.add(exportStakeWalletActivitys(stakeKeyReportHistory.getStakeKey(),
                                                    stakeKeyReportHistory.getIsFeesPaid(),
                                                    stakeLifeCycleFilterRequest));
    }

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getEventRegistration())) {
      exportContents.add(exportStakeRegistrations(stakeKeyReportHistory.getStakeKey(),
                                                  stakeLifeCycleFilterRequest));
    }

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getEventDelegation())) {
      exportContents.add(exportStakeDelegations(stakeKeyReportHistory.getStakeKey(),
                                                stakeLifeCycleFilterRequest));
    }

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getEventRewards())) {
      exportContents.add(exportStakeRewards(stakeKeyReportHistory.getStakeKey(),
                                            stakeLifeCycleFilterRequest));
    }

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getEventWithdrawal())) {
      exportContents.add(exportStakeWithdrawals(stakeKeyReportHistory.getStakeKey(),
                                                stakeLifeCycleFilterRequest));
    }

    if (Boolean.TRUE.equals(stakeKeyReportHistory.getEventDeregistration())) {
      exportContents.add(exportStakeDeregistrations(stakeKeyReportHistory.getStakeKey(),
                                                    stakeLifeCycleFilterRequest));
    }
    return exportContents;
  }

  private StakeLifeCycleFilterRequest getStakeLifeCycleFilterRequest(
      StakeKeyReportHistory stakeKeyReportHistory) {
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = new StakeLifeCycleFilterRequest();
    stakeLifeCycleFilterRequest.setFromDate(stakeKeyReportHistory.getFromDate());
    stakeLifeCycleFilterRequest.setToDate(stakeKeyReportHistory.getToDate());
    return stakeLifeCycleFilterRequest;
  }

  private ExportContent exportStakeWalletActivitys(String stakeKey, Boolean isFeesPaid,
                                                   StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeWalletActivityResponse> stakeWalletActivitys = stakeKeyLifeCycleService.getStakeWalletActivities(
            stakeKey, defaultPageable).getData()
        .stream()
        .filter(
            response -> isTimeInFilterRange(Date.from(response.getTime().toInstant(ZoneOffset.UTC)),
                                            stakeLifeCycleFilterRequest))
        .toList();

    return ExportContent.builder()
        .clazz(StakeWalletActivityResponse.class)
        .headerTitle(WALLET_ACTIVITY_TITLE)
        .lstColumn(StakeWalletActivityResponse.buildStakeWalletActivityColumn(isFeesPaid))
        .lstData(stakeWalletActivitys)
        .build();
  }

  private ExportContent exportStakeRegistrations(String stakeKey,
                                                 StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    List<StakeRegistrationLifeCycle> stakeRegistrations = stakeKeyLifeCycleService.getStakeRegistrations(
        stakeKey, stakeLifeCycleFilterRequest, defaultPageable).getData();
    return ExportContent.builder()
        .clazz(StakeRegistrationLifeCycle.class)
        .headerTitle(STAKE_KEY_REGISTRATION_TITLE)
        .lstColumn(StakeRegistrationLifeCycle.buildExportColumn())
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
        .lstColumn(StakeDelegationFilterResponse.buildExportColumn())
        .lstData(stakeDelegations)
        .build();
  }

  private ExportContent exportStakeRewards(String stakeKey,
                                           StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    Pageable rewardPageable = PageRequest.of(0, 1000, Sort.by("id").descending());
    List<StakeRewardResponse> stakeRewards = stakeKeyLifeCycleService.getStakeRewards(
            stakeKey, rewardPageable).getData()
        .stream()
        .filter(response -> isTimeInFilterRange(response.getTime(), stakeLifeCycleFilterRequest))
        .toList();
    return ExportContent.builder()
        .clazz(StakeRewardResponse.class)
        .headerTitle(REWARDS_DISTRIBUTION_TITLE)
        .lstColumn(StakeRewardResponse.buildExportColumn())
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
        .lstColumn(StakeWithdrawalFilterResponse.buildExportColumn())
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
        .lstColumn(StakeRegistrationLifeCycle.buildExportColumn())
        .lstData(stakeDeRegistrations)
        .build();
  }

  private String generateReportName(StakeKeyReportHistory stakeKeyReportHistory) {
    return "report_stake_" + stakeKeyReportHistory.getStakeKey() + "_" +
        DataUtil.instantToString(stakeKeyReportHistory.getFromDate().toInstant(), DATE_TIME_PATTERN)
        + "_" +
        DataUtil.instantToString(stakeKeyReportHistory.getToDate().toInstant(), DATE_TIME_PATTERN);
  }

  private String generateStorageKey(StakeKeyReportHistory stakeKeyReportHistory) {
    return stakeKeyReportHistory.getReportHistory().getId() + "_"
        + stakeKeyReportHistory.getReportHistory()
        .getReportName();
  }

  private Boolean isTimeInFilterRange(Date time,
                                      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest) {
    Date fromDate = stakeLifeCycleFilterRequest.getFromDate();
    Date toDate = stakeLifeCycleFilterRequest.getToDate();
    return time.compareTo(fromDate) >= BigInteger.ZERO.intValue()
        && time.compareTo(toDate) <= BigInteger.ZERO.intValue();
  }
}
