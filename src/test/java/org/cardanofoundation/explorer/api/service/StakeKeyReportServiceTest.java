package org.cardanofoundation.explorer.api.service;

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.mapper.StakeKeyReportMapper;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.StakeKeyReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyReportServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.ReportHistory;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
public class StakeKeyReportServiceTest {

  @Mock
  StakeKeyReportHistoryRepository stakeKeyReportHistoryRepository;

  @Mock
  StakeAddressRepository stakeAddressRepository;

  @InjectMocks
  StakeKeyReportServiceImpl stakeKeyReportService;

  @Mock
  StorageService storageService;

  @Mock
  StakeKeyReportMapper stakeKeyReportMapper;

  @Mock
  StakeKeyLifeCycleService stakeKeyLifeCycleService;

  @Test
  void generateStakeKeyReport_shouldThrowExceptionWhenNotFoundStakeAdress() {
    StakeKeyReportRequest request = StakeKeyReportRequest.builder()
        .stakeKey("any")
        .build();
    String username = "username";
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyReportService.generateStakeKeyReport(request, username));
  }

  @Test
  void generateStakeKeyReport_shouldGenerateStakeReportHistory() {
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    StakeKeyReportRequest request = StakeKeyReportRequest.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .build();
    String username = "username";

    StakeKeyReportHistory expect = StakeKeyReportHistory.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .reportHistory(ReportHistory.builder().username(username)
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();

    StakeKeyReportHistoryResponse responseExpect = StakeKeyReportHistoryResponse.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.STAKE_KEY)
        .build();

    when(stakeAddressRepository.findByView(anyString())).thenReturn(
        Optional.of(StakeAddress.builder().build()));

    when(stakeKeyReportHistoryRepository.save(any(StakeKeyReportHistory.class))).thenReturn(expect);
    when(stakeKeyReportMapper.toStakeKeyReportHistory(any(StakeKeyReportRequest.class))).thenReturn(
        expect);

    when(stakeKeyReportMapper.toStakeKeyReportHistoryResponse(expect))
        .thenReturn(responseExpect);

    var responseActual = stakeKeyReportService.generateStakeKeyReport(request, "username");
    Assertions.assertEquals(responseExpect.getStakeKey(), responseActual.getStakeKey());
    Assertions.assertEquals(responseExpect.getFromDate(), responseActual.getFromDate());
    Assertions.assertEquals(responseExpect.getToDate(), responseActual.getToDate());
    Assertions.assertEquals(responseExpect.getIsADATransfer(), responseActual.getIsADATransfer());
    Assertions.assertEquals(responseExpect.getEventRegistration(),
        responseActual.getEventRegistration());
    Assertions.assertEquals(responseExpect.getStatus(), responseActual.getStatus());
    Assertions.assertEquals(responseExpect.getType(), responseActual.getType());
  }

  @Test
  void getStakeKeyReportHistory_shouldReturnStakeKeyReportHistory() {
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    Pageable pageable = PageRequest.of(0, 1);
    String username = "username";
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();

    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = StakeKeyReportHistoryResponse.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.STAKE_KEY)
        .build();

    when(stakeKeyReportHistoryRepository.findByUsername(anyString(), any(Pageable.class)))
        .thenReturn(new PageImpl<>(List.of(stakeKeyReportHistory)));
    when(stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory))
        .thenReturn(stakeKeyReportHistoryResponse);

    var response = stakeKeyReportService.getStakeKeyReportHistory(username, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
        response.getData().get(0).getStakeKey());
    Assertions.assertEquals(fromDate, response.getData().get(0).getFromDate());
    Assertions.assertEquals(toDate, response.getData().get(0).getToDate());
    Assertions.assertEquals(Boolean.TRUE, response.getData().get(0).getIsADATransfer());
    Assertions.assertEquals(Boolean.TRUE, response.getData().get(0).getEventRegistration());
    Assertions.assertEquals(ReportStatus.IN_PROGRESS, response.getData().get(0).getStatus());
    Assertions.assertEquals(ReportType.STAKE_KEY, response.getData().get(0).getType());
  }

  @Test
  void getStakeKeyReportHistoryByStakeKey_shouldReturnStakeKeyReportHistory() {
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    Pageable pageable = PageRequest.of(0, 1);
    String username = "username";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();

    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = StakeKeyReportHistoryResponse.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.STAKE_KEY)
        .build();

    when(stakeKeyReportHistoryRepository.findByUsernameAndStakeKey(anyString(), anyString(),
        any(Pageable.class)))
        .thenReturn(new PageImpl<>(List.of(stakeKeyReportHistory)));
    when(stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory))
        .thenReturn(stakeKeyReportHistoryResponse);

    var response = stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(stakeKey, response.getData().get(0).getStakeKey());
    Assertions.assertEquals(fromDate, response.getData().get(0).getFromDate());
    Assertions.assertEquals(toDate, response.getData().get(0).getToDate());
    Assertions.assertEquals(Boolean.TRUE, response.getData().get(0).getIsADATransfer());
    Assertions.assertEquals(Boolean.TRUE, response.getData().get(0).getEventRegistration());
    Assertions.assertEquals(ReportStatus.IN_PROGRESS, response.getData().get(0).getStatus());
    Assertions.assertEquals(ReportType.STAKE_KEY, response.getData().get(0).getType());
  }

  @Test
  void exportStakeKeyReport_shouldThrowExceptionWhenReportHistoryNotFound() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyReportService.exportStakeKeyReport(reportId, username,
            exportType));
  }

  @Test
  void exportStakeKeyReport_shouldThrowExceptionWhenUsernameDoNotHavePermission() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username("otherUsername")
                .build())
            .build()));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyReportService.exportStakeKeyReport(reportId, username,
            exportType));
  }

  @Test
  void exportStakeKeyReport_shouldThrowExceptionWhenReportNotYetPersistToStorage() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .build()));

    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyReportService.exportStakeKeyReport(reportId, username,
            exportType));
  }

  @Test
  void exportStakeKeyReport_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    byte[] bytes = new byte[1];
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .storageKey("storageKey")
            .reportName("reportName")
            .status(ReportStatus.GENERATED)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(stakeKeyReportHistory));

    StakeKeyReportResponse expect = StakeKeyReportResponse.builder()
        .fileName("reportName" + exportType.getValue())
        .byteArrayInputStream(new ByteArrayInputStream(bytes))
        .build();

    when(storageService.downloadFile(anyString())).thenReturn(bytes);

    var response = stakeKeyReportService.exportStakeKeyReport(reportId, username, exportType);
    byte[] responseBytes = response.getByteArrayInputStream().readAllBytes();
    Assertions.assertEquals(expect.getFileName(), response.getFileName());
    Assertions.assertEquals(bytes.length, responseBytes.length);
    Assertions.assertEquals(bytes[0], responseBytes[0]);

  }

  @Test
  void getStakeKeyReportHistoryByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .storageKey("storageKey")
            .reportName("reportName")
            .status(ReportStatus.GENERATED)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();

    StakeKeyReportHistoryResponse expect = StakeKeyReportHistoryResponse.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .status(ReportStatus.GENERATED)
        .type(ReportType.STAKE_KEY)
        .build();

    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(stakeKeyReportHistory));
    when(stakeKeyReportMapper.toStakeKeyReportHistoryResponse(stakeKeyReportHistory))
        .thenReturn(expect);

    var response = stakeKeyReportService.getStakeKeyReportHistoryByReportId(reportId, username);
    Assertions.assertEquals(expect, response);
  }

  @Test
  void getStakeRegistrationsByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    LocalDateTime curTime = LocalDateTime.now();
    StakeRegistrationLifeCycle expect = StakeRegistrationLifeCycle.builder()
        .fee(BigInteger.TWO)
        .deposit(123L)
        .time(curTime)
        .txHash("txHash")
        .build();

    when(stakeKeyLifeCycleService.getStakeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getStakeRegistrationsByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getStakeDeRegistrationsByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    LocalDateTime curTime = LocalDateTime.now();
    StakeRegistrationLifeCycle expect = StakeRegistrationLifeCycle.builder()
        .fee(BigInteger.TWO)
        .deposit(123L)
        .time(curTime)
        .txHash("txHash")
        .build();

    when(stakeKeyLifeCycleService.getStakeDeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getStakeDeRegistrationsByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getStakeDelegationsByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    LocalDateTime curTime = LocalDateTime.now();
    StakeDelegationFilterResponse expect = StakeDelegationFilterResponse.builder()
        .fee(BigInteger.TWO)
        .outSum(BigInteger.TEN)
        .time(curTime)
        .txHash("txHash")
        .build();

    when(stakeKeyLifeCycleService.getStakeDelegations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getStakeDelegationsByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getStakeRewardsByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    StakeRewardResponse expect = StakeRewardResponse.builder()
        .epoch(1)
        .amount(BigInteger.TEN)
        .time(new Date()).build();

    when(stakeKeyLifeCycleService.getStakeRewards(anyString(), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getStakeRewardsByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getStakeWithdrawalsByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    LocalDateTime curTime = LocalDateTime.now();
    StakeWithdrawalFilterResponse expect = StakeWithdrawalFilterResponse.builder()
        .fee(BigInteger.TWO)
        .time(curTime)
        .txHash("txHash")
        .value(BigInteger.TEN)
        .build();

    when(stakeKeyLifeCycleService.getStakeWithdrawals(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getStakeWithdrawalsByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getWalletActivitiesByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    StakeWalletActivityResponse expect = new StakeWalletActivityResponse();
    expect.setFee(BigInteger.TWO);
    expect.setTime(LocalDateTime.now());
    expect.setType(StakeTxType.SENT);
    expect.setStatus(TxStatus.FAIL);
    expect.setTxHash("txHash");

    when(stakeKeyLifeCycleService.getStakeWalletActivities(anyString(), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getWalletActivitiesByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void getRewardActivitiesByReportId_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeKeyReportHistoryRepository.findById(any(Long.class))).thenReturn(
        Optional.of(StakeKeyReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                .username(username)
                .build())
            .fromDate(fromDate)
            .stakeKey(stakeKey)
            .toDate(toDate)
            .build()));

    StakeRewardActivityResponse expect = StakeRewardActivityResponse.builder()
        .epochNo(1)
        .amount(BigInteger.TEN)
        .time(new Date())
        .type(StakeRewardType.REWARD_RECEIVED)
        .build();

    when(stakeKeyLifeCycleService.getStakeRewardActivities(anyString(), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(new PageImpl<>(List.of(expect), pageable, 1)));

    var response = stakeKeyReportService.getRewardActivitiesByReportId(reportId, username,
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(expect, response.getData().get(0));
  }

  @Test
  void exportStakeKeyReport_shouldThrowExceptionWhenPersistFileToStorageFail() {
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    String username = "username";
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .isFeesPaid(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .eventDeregistration(Boolean.TRUE)
        .eventWithdrawal(Boolean.TRUE)
        .eventRewards(Boolean.TRUE)
        .eventDelegation(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .storageKey("storageKey")
            .reportName("reportName")
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();
    BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));
    when(stakeKeyLifeCycleService.getStakeWalletActivities(anyString(), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeRewards(anyString(), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeDelegations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeWithdrawals(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeDeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);

    doThrow(new RuntimeException()).when(storageService).uploadFile(any(), anyString());
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyReportService.exportStakeKeyReport(stakeKeyReportHistory));
  }

  @Test
  void exportStakeKeyReport_shouldSuccess() {
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = new Timestamp(System.currentTimeMillis());
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    String username = "username";
    StakeKeyReportHistory stakeKeyReportHistory = StakeKeyReportHistory.builder()
        .stakeKey(stakeKey)
        .fromDate(fromDate)
        .toDate(toDate)
        .isADATransfer(Boolean.TRUE)
        .isFeesPaid(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .eventDeregistration(Boolean.TRUE)
        .eventWithdrawal(Boolean.TRUE)
        .eventRewards(Boolean.TRUE)
        .eventDelegation(Boolean.TRUE)
        .reportHistory(ReportHistory.builder()
            .username(username)
            .storageKey("storageKey")
            .reportName("reportName")
            .status(ReportStatus.IN_PROGRESS)
            .type(ReportType.STAKE_KEY)
            .build())
        .build();
    BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));
    when(stakeKeyLifeCycleService.getStakeWalletActivities(anyString(), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeRewards(anyString(), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeDelegations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeWithdrawals(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);
    when(stakeKeyLifeCycleService.getStakeDeRegistrations(anyString(),
        any(StakeLifeCycleFilterRequest.class), any(Pageable.class)))
        .thenReturn(defaultBaseFilterResponse);

    doNothing().when(storageService).uploadFile(any(), anyString());
    stakeKeyReportService.exportStakeKeyReport(stakeKeyReportHistory);
  }
}
