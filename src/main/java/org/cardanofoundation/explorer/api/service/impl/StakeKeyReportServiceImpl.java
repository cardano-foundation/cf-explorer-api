package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.mapper.StakeKeyReportMapper;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.StakeKeyReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.KafkaService;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.service.StakeKeyLifeCycleService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import org.cardanofoundation.explorer.api.service.StorageService;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

import java.io.ByteArrayInputStream;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class StakeKeyReportServiceImpl implements StakeKeyReportService {

  public static final String MIN_TIME = "1970-01-01 00:00:00";
  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;
  private final StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;
  private final RewardRepository rewardRepository;
  private final StakeKeyReportMapper stakeKeyReportMapper;
  private final StakeAddressRepository stakeAddressRepository;
  private final StorageService storageService;
  private final KafkaService kafkaService;
  private final FetchRewardDataService fetchRewardDataService;
  private final ReportHistoryService reportHistoryService;

  @Override
  public StakeKeyReportHistoryResponse generateStakeKeyReport(
      StakeKeyReportRequest stakeKeyReportRequest, String username) {
    StakeKeyReportHistory stakeKeyReportHistory = save(stakeKeyReportRequest, username);
    kafkaService.sendReportHistory(stakeKeyReportHistory.getReportHistory());
    return stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory);
  }

  @Transactional
  public StakeKeyReportHistory save(StakeKeyReportRequest stakeKeyReportRequest, String username) {
    stakeAddressRepository.findByView(stakeKeyReportRequest.getStakeKey())
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportMapper.toStakeKeyReportHistory(
        stakeKeyReportRequest);

    if(Boolean.TRUE.equals(reportHistoryService.isLimitReached(username))){
      throw new BusinessException(BusinessCode.REPORT_LIMIT_REACHED);
    }

    if (DataUtil.isNullOrEmpty(stakeKeyReportRequest.getReportName())) {
      String reportName = generateReportName(stakeKeyReportHistory);
      stakeKeyReportHistory.getReportHistory().setReportName(reportName);
    }

    stakeKeyReportHistory.getReportHistory().setStatus(ReportStatus.IN_PROGRESS);
    stakeKeyReportHistory.getReportHistory().setType(ReportType.STAKE_KEY);
    stakeKeyReportHistory.getReportHistory().setUsername(username);
    return stakeKeyReportHistoryRepository.saveAndFlush(stakeKeyReportHistory);
  }

  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(String username,
                                                                                    ReportHistoryFilterRequest filterRequest,
                                                                                    Pageable pageable) {
    Timestamp timestamp = new Timestamp(Instant.now().minus(Duration.ofDays(7)).toEpochMilli());
    String reportName = DataUtil.makeLikeQuery(filterRequest.getReportName());
    Timestamp fromDate = Timestamp.valueOf(MIN_TIME);
    Timestamp toDate = Timestamp.from(Instant.now());
    if (!DataUtil.isNullOrEmpty(filterRequest.getFromDate())) {
      fromDate = Timestamp.from(filterRequest.getFromDate().toInstant());
    }
    if (!DataUtil.isNullOrEmpty(filterRequest.getToDate())) {
      toDate = Timestamp.from(filterRequest.getToDate().toInstant());
    }

    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse = stakeKeyReportHistoryRepository
        .getStakeKeyReportHistoryByFilter(reportName, fromDate, toDate, username, pageable)
        .map(stakeKeyReportHistory -> {
          StakeKeyReportHistoryResponse response = stakeKeyReportMapper
              .toStakeKeyReportHistoryResponse(stakeKeyReportHistory);

          if (response.getCreatedAt().before(timestamp)) {
            response.setStatus(ReportStatus.EXPIRED);
          }
          return response;
        });

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
    if (!ExportType.EXCEL.equals(exportType)) {
      throw new BusinessException(BusinessCode.EXPORT_TYPE_NOT_SUPPORTED);
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
    fetchReward(stakeKey);
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = getStakeLifeCycleFilterRequest(
        stakeKeyReportHistory);

    Page<StakeRewardResponse> stakeRewardResponses = rewardRepository
        .findRewardByStake(stakeKey,
                           Timestamp.from(stakeLifeCycleFilterRequest.getFromDate().toInstant()),
                           Timestamp.from(stakeLifeCycleFilterRequest.getToDate().toInstant()),
                           pageable);

    return new BaseFilterResponse<>(stakeRewardResponses);
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
    return stakeKeyLifeCycleService
        .getStakeWalletActivitiesByDateRange(stakeKey, stakeLifeCycleFilterRequest, pageable);
  }

  private StakeLifeCycleFilterRequest getStakeLifeCycleFilterRequest(
      StakeKeyReportHistory stakeKeyReportHistory) {
    StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = new StakeLifeCycleFilterRequest();
    stakeLifeCycleFilterRequest.setFromDate(stakeKeyReportHistory.getFromDate());
    stakeLifeCycleFilterRequest.setToDate(stakeKeyReportHistory.getToDate());
    return stakeLifeCycleFilterRequest;
  }

  private String generateReportName(StakeKeyReportHistory stakeKeyReportHistory) {
    String dateTimePattern = "yyyyMMdd";
    return "report_stake_" + stakeKeyReportHistory.getStakeKey() + "_" +
        DataUtil.localDateTimeToString(stakeKeyReportHistory.getFromDate().toLocalDateTime(),
                                       dateTimePattern)
        + "_" +
        DataUtil.localDateTimeToString(stakeKeyReportHistory.getFromDate().toLocalDateTime(),
                                       dateTimePattern);
  }

  private void fetchReward(String stakeKey) {
    stakeAddressRepository.findByView(stakeKey)
        .orElseThrow(() -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));
    if (!fetchRewardDataService.checkRewardAvailable(stakeKey)) {
      boolean fetchRewardResponse = fetchRewardDataService.fetchReward(stakeKey);
      if (!fetchRewardResponse) {
        throw new FetchRewardException(BusinessCode.FETCH_REWARD_ERROR);
      }
    }
  }
}
