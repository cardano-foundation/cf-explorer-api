package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.repository.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.PoolReportRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolReportServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.*;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.io.ByteArrayInputStream;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class PoolReportServiceTest {

  @Mock
  PoolReportRepository poolReportRepository;
  @Mock
  EpochStakeRepository epochStakeRepository;
  @Mock
  StorageService storageService;
  @Mock
  PoolLifecycleService poolLifecycleService;
  @Mock
  PoolHashRepository poolHashRepository;
  @InjectMocks
  PoolReportServiceImpl poolReportService;

  @Test
  void create_shouldThrowExceptionWhenNotFoundStakeAdress() {
    PoolReportCreateRequest request = PoolReportCreateRequest.builder()
        .poolId("any")
        .build();
    String username = "username";
    when(poolHashRepository.findByView(anyString())).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
                            () -> poolReportService.create(request, username));
  }

  @Test
  void create_shouldCreate() {
    PoolReportCreateRequest request = PoolReportCreateRequest.builder()
        .poolId("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .isFeesPaid(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventPoolUpdate(true)
        .eventReward(true)
        .epochRanges(new Integer[]{300, 410})
        .build();
    String username = "username";

    PoolReportHistory saved = PoolReportHistory.builder()
        .isPoolSize(true)
        .isFeesPaid(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .beginEpoch(300)
        .endEpoch(410)
        .id(1L)
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .reportHistory(ReportHistory.builder()
                           .id(1L)
                           .reportName(
                               "report_pool_pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                           .status(ReportStatus.IN_PROGRESS)
                           .type(ReportType.POOL_ID)
                           .username(username)
                           .createdAt(new Timestamp(System.currentTimeMillis()))
                           .build())
        .build();

    when(poolHashRepository.findByView(any(String.class))).thenReturn(Optional.of(new PoolHash()));
    when(poolReportRepository.save(any(PoolReportHistory.class))).thenReturn(saved);

    Assertions.assertTrue(poolReportService.create(request, username));
  }

  @Test
  void export_shouldThrowExceptionWhenReportNotYetPersistToStorage() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(
        PoolReportHistory.builder()
            .reportHistory(ReportHistory.builder()
                               .username(username)
                               .build())
            .build());

    Assertions.assertThrows(BusinessException.class,
                            () -> poolReportService.export(reportId, exportType, username));
  }

  @Test
  void export_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    ExportType exportType = ExportType.EXCEL;
    String poolId = "pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj";
    byte[] bytes = new byte[1];
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);

    PoolReportExportResponse expect = PoolReportExportResponse.builder()
        .fileName("reportName" + exportType.getValue())
        .byteArrayInputStream(new ByteArrayInputStream(bytes))
        .build();

    when(storageService.downloadFile(anyString())).thenReturn(bytes);

    var response = poolReportService.export(reportId, exportType, username);
    byte[] responseBytes = response.getByteArrayInputStream().readAllBytes();
    Assertions.assertEquals(expect.getFileName(), response.getFileName());
    Assertions.assertEquals(bytes.length, responseBytes.length);
    Assertions.assertEquals(bytes[0], responseBytes[0]);
  }

  @Test
  void exportDirect_shouldThrowExceptionWhenPersistFileToStorageFail() {
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.POOL_ID)
                           .build())
        .build();
    BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));

    when(epochStakeRepository.getEpochSizeByPoolReport(anyString(), anyInt(), anyInt())).thenReturn(
        new ArrayList<>());
    when(poolLifecycleService.registrationList(anyString(), any())).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.poolUpdateList(anyString(), any())).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.listReward(anyString(), any())).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.deRegistration(anyString(), anyString(), any(Date.class),
                                             any(Date.class), any(Pageable.class))).thenReturn(
        defaultBaseFilterResponse);
    doThrow(new RuntimeException()).when(storageService).uploadFile(any(), anyString());
    poolReportService.exportDirect(poolReport);
  }

  @Test
  void exportDirect_shouldSuccess() {
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .isPoolSize(true)
        .poolView("pool1")
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.POOL_ID)
                           .build())
        .build();
    BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));
    when(poolLifecycleService.registrationList(anyString(), any(Pageable.class))).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.poolUpdateList(anyString(), any(Pageable.class))).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.listReward(anyString(), any(Pageable.class))).thenReturn(
        defaultBaseFilterResponse);
    when(poolLifecycleService.deRegistration(any(), any(), any(),
                                             any(), any())).thenReturn(
        defaultBaseFilterResponse);
    doNothing().when(storageService).uploadFile(any(), anyString());
    Assertions.assertDoesNotThrow(() -> poolReportService.exportDirect(poolReport));
  }

  @Test
  void fetchEpochSize_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);
    Page<PoolReportProjection> poolReportProjections = new PageImpl<>(List.of(),
                                                                      PageRequest.of(0, 1), 0);
    when(epochStakeRepository.getEpochSizeByPoolReport(anyString(), anyInt(), anyInt(),
                                                       any())).thenReturn(poolReportProjections);
    var expect = new BaseFilterResponse<>(poolReportProjections, List.of());

    var response = poolReportService.fetchEpochSize(reportId, PageRequest.of(0, 1), username);

    Assertions.assertEquals(expect.getData(), response.getData());
    Assertions.assertEquals(expect.getTotalItems(), response.getTotalItems());
    Assertions.assertEquals(expect.getCurrentPage(), response.getCurrentPage());
    Assertions.assertEquals(expect.getTotalPages(), response.getTotalPages());
  }

  @Test
  void fetchPoolRegistration_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);
    BaseFilterResponse<TabularRegisResponse> tabularRegisResponse = new BaseFilterResponse<>();
    when(poolLifecycleService.registrationList(any(), any())).thenReturn(tabularRegisResponse);

    var response = poolReportService.fetchPoolRegistration(reportId, PageRequest.of(0, 1),
                                                           username);
    Assertions.assertEquals(tabularRegisResponse, response);
  }

  @Test
  void fetchPoolUpdate_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);
    BaseFilterResponse<PoolUpdateDetailResponse> poolUpdateDetailResponse = new BaseFilterResponse<>();
    when(poolLifecycleService.poolUpdateList(any(), any())).thenReturn(poolUpdateDetailResponse);

    var response = poolReportService.fetchPoolUpdate(reportId, PageRequest.of(0, 1), username);
    Assertions.assertEquals(poolUpdateDetailResponse, response);
  }

  @Test
  void fetchRewardsDistribution_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);
    BaseFilterResponse<RewardResponse> rewardResponse = new BaseFilterResponse<>();
    when(poolLifecycleService.listReward(any(), any())).thenReturn(rewardResponse);

    var response = poolReportService.fetchRewardsDistribution(reportId, PageRequest.of(0, 1),
                                                              username);
    Assertions.assertEquals(rewardResponse, response);
  }

  @Test
  void fetchDeregistraion_shouldReturnResponse() {
    Long reportId = 1L;
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(poolReport);
    BaseFilterResponse<DeRegistrationResponse> deRegistrationResponse = new BaseFilterResponse<>();
    when(poolLifecycleService.deRegistration(any(), any(), any(), any(), any())).thenReturn(
        deRegistrationResponse);

    var response = poolReportService.fetchDeregistraion(reportId, PageRequest.of(0, 1), username);
    Assertions.assertEquals(deRegistrationResponse, response);
  }

  @Test
  void list_shouldReturnResponse() {
    String username = "username";
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();
    when(poolReportRepository.findByUsername(any(), any())).thenReturn(
        new PageImpl<>(List.of(poolReport)));

    var response = poolReportService.list(PageRequest.of(0, 1), username);

    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());


    Assertions.assertEquals(PoolReportListResponse.toDomain(poolReport), response.getData().get(0));
  }

  @Test
  void detail_shouldReturnDetail(){
    String username = "username";
    Long reportId = 1L;
    PoolReportHistory poolReport = PoolReportHistory.builder()
        .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
        .isPoolSize(true)
        .eventRegistration(true)
        .eventDeregistration(true)
        .eventReward(true)
        .eventPoolUpdate(true)
        .isFeesPaid(true)
        .beginEpoch(300)
        .endEpoch(410)
        .reportHistory(ReportHistory.builder()
                           .username(username)
                           .storageKey("storageKey")
                           .reportName("reportName")
                           .status(ReportStatus.GENERATED)
                           .type(ReportType.STAKE_KEY)
                           .build())
        .build();

    when(poolReportRepository.findByUsernameAndId(username, reportId)).thenReturn(poolReport);

    var response = poolReportService.detail(reportId, username);
    Assertions.assertEquals(poolReport, response);
  }
}
