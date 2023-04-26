package com.cardano.explorer.service.impl;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.StakeKeyReportMapper;
import com.cardano.explorer.model.request.report.StakeKeyReport;
import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportHistoryResponse;
import com.cardano.explorer.model.response.report.StakeKeyReportResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWalletActivityResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeKeyReportHistoryRepository;
import com.cardano.explorer.service.StakeKeyLifeCycleService;
import com.cardano.explorer.service.StakeKeyReportService;
import com.cardano.explorer.util.CSVColumn;
import com.cardano.explorer.util.CSVHelper;
import com.cardano.explorer.util.DataUtil;
import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import com.sotatek.cardano.common.entity.StakingLifeCycleEvent;
import com.sotatek.cardano.common.enumeration.StakeKeyReportStatus;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
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

  private final StakeKeyLifeCycleService stakeKeyLifeCycleService;

  private final StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;

  private final StakeKeyReportMapper stakeKeyReportMapper;

  private final StakeAddressRepository stakeAddressRepository;

  private final AmazonS3 s3Client;
  
  private final Pageable defaultPageable = PageRequest.of(0, 1000);

  @Value("${cloud.aws.s3.bucket.name}")
  String bucketName;

  @Override
  public StakeKeyReportHistoryResponse generateStakeKeyReport(StakeKeyReport stakeKeyReport) {

    stakeAddressRepository.findByView(stakeKeyReport.getStakeKey())
        .orElseThrow(
            () -> new BusinessException(BusinessCode.STAKE_ADDRESS_NOT_FOUND));

    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportMapper.toStakeKeyReportHistory(
        stakeKeyReport);

    String reportName = getReportName(stakeKeyReport);
    stakeKeyReportHistory.setReportName(reportName);
    stakeKeyReportHistory.setStatus(StakeKeyReportStatus.IN_PROGRESS);
    stakeKeyReportHistory = stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);
    return stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory);
  }

  private StakeKeyReportResponse exportStakeKeyReport(StakeKeyReportHistory stakeKeyReportHistory) {
    String csvFileName = stakeKeyReportHistory.getReportName();
    try {
      InputStream inputStream = InputStream.nullInputStream();
      Set<StakingLifeCycleEvent> stakingLifeCycleEvents = stakeKeyReportHistory.getStakingLifeCycleEvents()
          .stream()
          .sorted(Comparator.comparingInt(o -> o.getType().getValue()))
          .collect(Collectors.toCollection(LinkedHashSet::new));
      StakeLifeCycleFilterRequest stakeLifeCycleFilterRequest = new StakeLifeCycleFilterRequest();
      stakeLifeCycleFilterRequest.setFromDate(stakeKeyReportHistory.getFromDate());
      stakeLifeCycleFilterRequest.setToDate(stakeKeyReportHistory.getToDate());
      if (stakeKeyReportHistory.getIsADATransfer()) {
        inputStream = CSVHelper.writeContent("ADA Transfer");
        List<StakeWalletActivityResponse> stakeWalletActivitys = stakeKeyLifeCycleService.getStakeWalletActivities(
            stakeKeyReportHistory.getStakeKey(),
            defaultPageable).getData();

        List<StakeRewardActivityResponse> stakeRewardActivitys = stakeKeyLifeCycleService.getStakeRewardActivities(
            stakeKeyReportHistory.getStakeKey(),
            defaultPageable).getData();

        String walletActivityTitle = "Wallet Activity";
        inputStream = CSVHelper.writeContent(stakeWalletActivitys, buildStakeWalletActivityColumn(),
            inputStream.readAllBytes(), walletActivityTitle);

        String rewardActivityTitle = "Reward Activity";
        inputStream = CSVHelper.writeContent(stakeRewardActivitys, buildStakeRewardActivityColumn(),
            inputStream.readAllBytes(), rewardActivityTitle);
      }

      if (!stakingLifeCycleEvents.isEmpty()) {
        inputStream = CSVHelper.writeContent(inputStream.readAllBytes(), "Stake Key Life Cycle");
      }

      for (StakingLifeCycleEvent stakingLifeCycleEvent : stakingLifeCycleEvents) {
        switch (stakingLifeCycleEvent.getType()) {
          case REGISTRATION:
            List<StakeRegistrationLifeCycle> stakeRegistrations = stakeKeyLifeCycleService.getStakeRegistrations(
                    stakeKeyReportHistory.getStakeKey(), stakeLifeCycleFilterRequest,
                    defaultPageable)
                .getData();
            String registrationTitle = "Stake Key Registration";
            inputStream = CSVHelper.writeContent(stakeRegistrations, buildStakeRegistrationColumn(),
                inputStream.readAllBytes(), registrationTitle);
            break;
          case DELEGATION:
            List<StakeDelegationFilterResponse> stakeDelegations = stakeKeyLifeCycleService.getStakeDelegations(
                    stakeKeyReportHistory.getStakeKey(), stakeLifeCycleFilterRequest,
                    defaultPageable)
                .getData();
            String delegationTitle = "Delegation History";
            inputStream = CSVHelper.writeContent(stakeDelegations, buildStakeDelegationColumn(),
                inputStream.readAllBytes(), delegationTitle);
            break;
          case REWARDS:
            List<StakeRewardResponse> stakeRewards = stakeKeyLifeCycleService.getStakeRewards(
                stakeKeyReportHistory.getStakeKey(), defaultPageable).getData();
            String rewardTitle = "Rewards Distribution";
            inputStream = CSVHelper.writeContent(stakeRewards, buildStakeRewardColumn(),
                inputStream.readAllBytes(), rewardTitle);
            break;
          case WITHDRAWAL:
            List<StakeWithdrawalFilterResponse> stakeWithdrawals = stakeKeyLifeCycleService.getStakeWithdrawals(
                    stakeKeyReportHistory.getStakeKey(), stakeLifeCycleFilterRequest,
                    defaultPageable)
                .getData();
            String withdrawalTitle = "Withdrawal History";
            inputStream = CSVHelper.writeContent(stakeWithdrawals, buildStakeWithdrawalColumn(),
                inputStream.readAllBytes(), withdrawalTitle);
            break;
          case DEREGISTRATION:
            List<StakeRegistrationLifeCycle> stakeDeRegistrations = stakeKeyLifeCycleService.getStakeDeRegistrations(
                    stakeKeyReportHistory.getStakeKey(), stakeLifeCycleFilterRequest,
                    defaultPageable)
                .getData();
            String deRegistrationTitle = "Stake Key Deregistration";
            inputStream = CSVHelper.writeContent(stakeDeRegistrations,
                buildStakeDeRegistrationColumn(), inputStream.readAllBytes(), deRegistrationTitle);
            break;
          default:
            break;
        }
      }
      byte[] bytes = inputStream.readAllBytes();
      uploadFile(bytes, csvFileName);
      stakeKeyReportHistory.setStatus(StakeKeyReportStatus.GENERATED);
      stakeKeyReportHistoryRepository.save(stakeKeyReportHistory);
      return StakeKeyReportResponse.builder()
          .fileName(csvFileName)
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }


  @Override
  public BaseFilterResponse<StakeKeyReportHistoryResponse> getStakeKeyReportHistory(
      String stakeKey, Pageable pageable) {
    Page<StakeKeyReportHistoryResponse> stakeKeyReportHistoriesResponse = stakeKeyReportHistoryRepository.findByStakeKey(
            stakeKey, pageable)
        .map(stakeKeyReportMapper::toStakeKeyReportHistoryResponse);

    return new BaseFilterResponse<>(stakeKeyReportHistoriesResponse);
  }

  @Override
  public StakeKeyReportResponse exportStakeKeyReport(Long reportId) {
    StakeKeyReportHistory stakeKeyReportHistory = stakeKeyReportHistoryRepository.findById(reportId)
        .orElseThrow(() -> new RuntimeException("Stake Key Report History not found"));

    if (StakeKeyReportStatus.IN_PROGRESS.equals(stakeKeyReportHistory.getStatus())) {
      return exportStakeKeyReport(stakeKeyReportHistory);
    } else {
      byte[] bytes = downloadFile(stakeKeyReportHistory.getReportName());
      return StakeKeyReportResponse.builder()
          .fileName(stakeKeyReportHistory.getReportName())
          .byteArrayInputStream(new ByteArrayInputStream(bytes))
          .build();
    }
  }


  private List<CSVColumn> buildStakeRegistrationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("txHash", "Transaction Hash"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("deposit", "Hold"));
    columns.add(new CSVColumn("fee", "Fees"));
    return columns;
  }

  private List<CSVColumn> buildStakeDelegationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("txHash", "Transaction Hash"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("outSum", "Fees"));
    return columns;
  }

  private List<CSVColumn> buildStakeRewardColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("epoch", "Epoch"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("amount", "Rewards Paid"));
    return columns;
  }

  private List<CSVColumn> buildStakeWithdrawalColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("txHash", "Transaction Hash"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("value", "Net Amount"));
    return columns;
  }

  private List<CSVColumn> buildStakeDeRegistrationColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("txHash", "Transaction Hash"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("deposit", "Hold"));
    columns.add(new CSVColumn("fee", "Fees"));
    return columns;
  }

  private List<CSVColumn> buildStakeWalletActivityColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("txHash", "Transaction Hash"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("amount", "Amount ADA"));
    columns.add(new CSVColumn("type", "Transaction Type"));
    columns.add(new CSVColumn("status", "Status"));
    return columns;
  }

  private List<CSVColumn> buildStakeRewardActivityColumn() {
    List<CSVColumn> columns = new ArrayList<>();
    columns.add(new CSVColumn("epochNo", "Epoch"));
    columns.add(new CSVColumn("time", "Timestamp"));
    columns.add(new CSVColumn("amount", "Amount ADA"));
    columns.add(new CSVColumn("type", "Transaction Type"));
    return columns;
  }

  private String instanceToString(Instant instant) {
    return DataUtil.instantToString(instant, "yyyyMMdd");
  }

  private String getReportName(StakeKeyReport stakeKeyReport) {
    return "report_stake_" + stakeKeyReport.getStakeKey() + "_" +
        instanceToString(stakeKeyReport.getFromDate().toInstant()) + "_" + instanceToString(
        stakeKeyReport.getToDate().toInstant());
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

  @Override
  public void deleteFile(String fileName, String fileType) {
  }

}
