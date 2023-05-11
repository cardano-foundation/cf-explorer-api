package org.cardanofoundation.explorer.api.service;

import com.amazonaws.services.s3.AmazonS3;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
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
    private PoolReportRepository poolReportRepository;
    @Mock
    private EpochStakeRepository epochStakeRepository;
    @Mock
    private StorageService storageService;
    @Mock
    private PoolLifecycleService poolLifecycleService;
    @Mock
    private PoolHashRepository poolHashRepository;
    @Mock
    private AmazonS3 s3Client;
    @InjectMocks
    private PoolReportServiceImpl poolReportService;

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
    void create_throwSave() {
        PoolReportCreateRequest request = PoolReportCreateRequest.builder()
                .poolId("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .reportName("Report name")
                .isPoolSize(true)
                .isFeesPaid(true)
                .isPoolUpdate(true)
                .epochRanges(new Integer[]{300, 410})
                .build();
        String username = "username";

        when(poolReportRepository.save(any())).thenThrow();

        Assertions.assertThrows(BusinessException.class, () -> poolReportService.create(request, username));
    }

    @Test
    void create_shouldCreate() {
        PoolReportCreateRequest request = PoolReportCreateRequest.builder()
                .poolId("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .reportName("Report name")
                .isPoolSize(true)
                .isFeesPaid(true)
                .isRegistration(true)
                .isDeregistration(true)
                .isReward(true)
                .isPoolUpdate(true)
                .epochRanges(new Integer[]{300, 410})
                .build();
        String username = "username";

        PoolReport saved = PoolReport.builder()
                .isPoolSize(true)
                .isFeesPaid(true)
                .isRegistration(true)
                .isDeregistration(true)
                .isReward(true)
                .isPoolUpdate(true)
                .beginEpoch(300)
                .endEpoch(410)
                .id(1L)
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .reportName("Report name")
                .reportHistory(ReportHistory.builder()
                        .id(1L)
                        .reportName("report_pool_pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                        .status(ReportStatus.IN_PROGRESS)
                        .type(ReportType.POOL_ID)
                        .username(username)
                        .createdAt(new Timestamp(System.currentTimeMillis()))
                        .build())
                .build();

        BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
                new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));
        when(poolHashRepository.findByView(any(String.class))).thenReturn(Optional.of(new PoolHash()));
        when(poolReportRepository.save(any(PoolReport.class))).thenReturn(saved);
        when(epochStakeRepository.getEpochSizeByPoolReport(any(String.class), any(Integer.class), any(Integer.class))).thenReturn(new ArrayList<>());
        when(poolLifecycleService.registrationList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.poolUpdateList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.listReward(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.deRegistration(any(String.class), any(String.class), any(Date.class), any(Date.class), any())).thenReturn(defaultBaseFilterResponse);
        doNothing().when(storageService).uploadFile(any(), anyString());

        Assertions.assertTrue(poolReportService.create(request, username));
    }

    @Test
    void export_shouldThrowExceptionWhenReportHistoryNotFound() {
        Long reportId = 1L;
        String username = "username";
        when(poolReportRepository.findByUsernameAndId(any(String.class), any(Long.class))).thenReturn(PoolReport.builder().build());
        Assertions.assertThrows(BusinessException.class,
                () -> poolReportService.export(reportId, ExportType.CSV, username));
    }

    @Test
    void export_shouldThrowExceptionWhenReportNotYetPersistToStorage() {
        Long reportId = 1L;
        String username = "username";
        ExportType exportType = ExportType.EXCEL;
        when(poolReportRepository.findByUsernameAndId(any(), any())).thenReturn(
                PoolReport.builder()
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
        PoolReport poolReport = PoolReport.builder()
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
        PoolReport poolReport = PoolReport.builder()
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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
        BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
                new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));

        when(epochStakeRepository.getEpochSizeByPoolReport(any(String.class), any(Integer.class), any(Integer.class))).thenReturn(new ArrayList<>());
        when(poolLifecycleService.registrationList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.poolUpdateList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.listReward(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.deRegistration(any(String.class), any(String.class), any(Date.class), any(Date.class), any())).thenReturn(defaultBaseFilterResponse);
        doThrow(new RuntimeException()).when(storageService).uploadFile(any(), anyString());
        Assertions.assertThrows(RuntimeException.class,
                () -> poolReportService.exportDirect(poolReport));
    }

    @Test
    void exportDirect_shouldSuccess() {
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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
        BaseFilterResponse defaultBaseFilterResponse = new BaseFilterResponse<>(
                new PageImpl<>(List.of(), PageRequest.of(0, 1), 0));

        when(epochStakeRepository.getEpochSizeByPoolReport(any(String.class), any(Integer.class), any(Integer.class))).thenReturn(new ArrayList<>());
        when(poolLifecycleService.registrationList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.poolUpdateList(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.listReward(any(String.class), any())).thenReturn(defaultBaseFilterResponse);
        when(poolLifecycleService.deRegistration(any(String.class), any(String.class), any(Date.class), any(Date.class), any())).thenReturn(defaultBaseFilterResponse);
        doNothing().when(storageService).uploadFile(any(), anyString());
        when(poolReportRepository.save(any())).thenReturn(PoolReport.builder().build());

        poolReportService.exportDirect(poolReport);
    }

    @Test
    void fetchEpochSize_shouldReturnResponse() {
        String reportId = "1";
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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
        Page<PoolReportProjection> poolReportProjections = new PageImpl<>(List.of(), PageRequest.of(0, 1), 0);
        when(epochStakeRepository.getEpochSizeByPoolReport(anyString(), anyInt(), anyInt(), any())).thenReturn(poolReportProjections);
        var expect = new BaseFilterResponse<>(poolReportProjections, List.of());

        var response = poolReportService.fetchEpochSize(reportId, PageRequest.of(0, 1), username);

        Assertions.assertEquals(expect.getData(), response.getData());
        Assertions.assertEquals(expect.getTotalItems(), response.getTotalItems());
        Assertions.assertEquals(expect.getCurrentPage(), response.getCurrentPage());
        Assertions.assertEquals(expect.getTotalPages(), response.getTotalPages());
    }

    @Test
    void fetchPoolRegistration_shouldReturnResponse() {
        String reportId = "1";
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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

        var response = poolReportService.fetchPoolRegistration(reportId, PageRequest.of(0, 1), username);
        Assertions.assertEquals(tabularRegisResponse, response);
    }

    @Test
    void fetchPoolUpdate_shouldReturnResponse() {
        String reportId = "1";
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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
        String reportId = "1";
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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

        var response = poolReportService.fetchRewardsDistribution(reportId, PageRequest.of(0, 1), username);
        Assertions.assertEquals(rewardResponse, response);
    }

    @Test
    void fetchDeregistraion_shouldReturnResponse() {
        String reportId = "1";
        String username = "username";
        PoolReport poolReport = PoolReport.builder()
                .poolView("pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj")
                .isPoolSize(true)
                .isRegistration(true)
                .isPoolUpdate(true)
                .isReward(true)
                .isDeregistration(true)
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
        when(poolLifecycleService.deRegistration(any(), any(), any(), any(), any())).thenReturn(deRegistrationResponse);

        var response = poolReportService.fetchDeregistraion(reportId, PageRequest.of(0, 1), username);
        Assertions.assertEquals(deRegistrationResponse, response);
    }
}
