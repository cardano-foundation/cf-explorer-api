package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.Mockito;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.interceptor.auth.UserPrincipal;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.LifeCycleRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.common.entity.explorer.PoolReportHistory;

@WebMvcTest(PoolReportController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class PoolReportControllerTest {
  @Autowired private MockMvc mockMvc;

  @MockBean private PoolReportService poolReportService;

  @MockBean private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testCreatePoolReport() throws Exception {
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    PoolReportCreateRequest bodyRequest =
        PoolReportCreateRequest.builder()
            .poolId("1")
            .isPoolSize(false)
            .reportName("reportName")
            .build();

    when(poolReportService.create(bodyRequest, userPrincipal)).thenReturn(true);

    mockMvc
        .perform(
            post("/api/v1/pool-report/create")
                .contentType(MediaType.APPLICATION_JSON)
                .content(
                    asJsonString(
                        Map.of("reportName", "reportName", "poolId", "1", "isPoolSize", "false")))
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    }))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").value(true));

    verify(poolReportService).create(bodyRequest, userPrincipal);
  }

  @Test
  void testListPoolReport() throws Exception {
    PoolReportListResponse poolReportListResponse =
        PoolReportListResponse.builder()
            .reportId(1L)
            .reportName("reportName")
            .isPoolSize(true)
            .build();
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();

    when(poolReportService.list(any(Pageable.class), any(), any(ReportHistoryFilterRequest.class)))
        .thenReturn(new BaseFilterResponse<>(List.of(poolReportListResponse), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/list")
                .param("page", "0")
                .param("size", "1")
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    }))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].reportId").value("1"))
        .andExpect(jsonPath("$.data[0].reportName").value("reportName"));

    verify(poolReportService)
        .list(any(Pageable.class), any(), any(ReportHistoryFilterRequest.class));
  }

  @Test
  void testDetailEpochSizePoolReport() throws Exception {
    PoolReportDetailResponse.EpochSize epochSize =
        PoolReportDetailResponse.EpochSize.builder()
            .epoch("1")
            .size(BigDecimal.ONE)
            .fee(BigInteger.ONE)
            .build();
    Long reportId = 263L;
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();

    when(poolReportService.fetchEpochSize(
            reportId, PageRequest.of(0, 1), userPrincipal.getUsername()))
        .thenReturn(new BaseFilterResponse<>(List.of(epochSize), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}/epoch-size", reportId)
                .param("page", "0")
                .param("size", "1")
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    }))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists());

    verify(poolReportService)
        .fetchEpochSize(reportId, PageRequest.of(0, 1), userPrincipal.getUsername());
  }

  @Test
  void testExportExcelFile() throws Exception {
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    Long reportId = 1L;
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream("test".getBytes());
    PoolReportExportResponse response =
        PoolReportExportResponse.builder()
            .fileName("stake-key-report.csv")
            .byteArrayInputStream(byteArrayInputStream)
            .build();

    when(poolReportService.export(anyLong(), any(ExportType.class), anyString()))
        .thenReturn(response);

    MvcResult result =
        mockMvc
            .perform(
                get("/api/v1/pool-report/detail/{reportId}/export", reportId)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("exportType", ExportType.CSV.name())
                    .with(
                        request -> {
                          request.setAttribute("user", userPrincipal);
                          return request;
                        }))
            .andExpect(status().isOk())
            .andReturn();

    Assertions.assertEquals(4, result.getResponse().getContentLength());
    Assertions.assertTrue(
        result
            .getResponse()
            .getHeader("Content-Disposition")
            .split("=")[1]
            .contains(response.getFileName()));
    Assertions.assertEquals(
        MediaType.APPLICATION_OCTET_STREAM_VALUE, result.getResponse().getContentType());
    verify(poolReportService).export(anyLong(), any(ExportType.class), anyString());
  }

  @Test
  void testDetailPoolRegistration() throws Exception {
    Long reportId = 1L;
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    PoolRegistrationProjection projection = Mockito.mock(PoolRegistrationProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getTxHash()).thenReturn("txHash");
    when(projection.getFee()).thenReturn(BigInteger.ONE);
    when(projection.getDeposit()).thenReturn(BigInteger.ONE);
    TabularRegisResponse tabularRegisResponse = new TabularRegisResponse(projection);

    when(poolReportService.fetchPoolRegistration(
            reportId, PageRequest.of(0, 1), userPrincipal.getUsername()))
        .thenReturn(new BaseFilterResponse<>(List.of(tabularRegisResponse), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}/pool-registration", reportId)
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    })
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].txHash").value("txHash"));

    verify(poolReportService)
        .fetchPoolRegistration(reportId, PageRequest.of(0, 1), userPrincipal.getUsername());
  }

  @Test
  void testDetailPoolUpdate() throws Exception {
    Long reportId = 1L;
    PoolUpdateDetailProjection projection = Mockito.mock(PoolUpdateDetailProjection.class);
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getCost()).thenReturn(BigInteger.ONE);
    PoolUpdateDetailResponse response = new PoolUpdateDetailResponse(projection);

    when(poolReportService.fetchPoolUpdate(
            reportId, PageRequest.of(0, 1), userPrincipal.getUsername()))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}/pool-update", reportId)
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    })
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].poolName").value("name"));

    verify(poolReportService)
        .fetchPoolUpdate(reportId, PageRequest.of(0, 1), userPrincipal.getUsername());
  }

  @Test
  void testDetailRewardsDistribution() throws Exception {
    Long reportId = 1L;
    LifeCycleRewardProjection projection = Mockito.mock(LifeCycleRewardProjection.class);
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    when(projection.getEpochNo()).thenReturn(1);
    when(projection.getAddress()).thenReturn("address");
    when(projection.getAmount()).thenReturn(BigInteger.ONE);
    RewardResponse response = new RewardResponse(projection);

    when(poolReportService.fetchRewardsDistribution(
            reportId, PageRequest.of(0, 1), userPrincipal.getUsername()))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}/rewards-distribution", reportId)
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    })
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].epochNo").value("1"));

    verify(poolReportService)
        .fetchRewardsDistribution(reportId, PageRequest.of(0, 1), userPrincipal.getUsername());
  }

  @Test
  void testDetailDeregistration() throws Exception {
    Long reportId = 1L;
    PoolDeRegistrationProjection projection = Mockito.mock(PoolDeRegistrationProjection.class);
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    when(projection.getPoolId()).thenReturn("1");
    when(projection.getRetiringEpoch()).thenReturn(1);
    when(projection.getTxHash()).thenReturn("txHash");
    DeRegistrationResponse response = new DeRegistrationResponse(projection);

    when(poolReportService.fetchDeregistraion(
            reportId, PageRequest.of(0, 1), userPrincipal.getUsername()))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}/deregistration", reportId)
                .param("page", "0")
                .param("size", "1")
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    }))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].txHash").value("txHash"));

    verify(poolReportService)
        .fetchDeregistraion(reportId, PageRequest.of(0, 1), userPrincipal.getUsername());
  }

  @Test
  void testDetailPoolReport() throws Exception {
    Long reportId = 1L;
    UserPrincipal userPrincipal = UserPrincipal.builder().username("username").build();
    PoolReportHistory response =
        PoolReportHistory.builder().id(1L).poolView("view").isPoolSize(false).build();

    when(poolReportService.detail(reportId, userPrincipal.getUsername())).thenReturn(response);

    mockMvc
        .perform(
            get("/api/v1/pool-report/detail/{reportId}", reportId)
                .with(
                    request -> {
                      request.setAttribute("user", userPrincipal);
                      return request;
                    }))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.poolView").value("view"));

    verify(poolReportService).detail(reportId, userPrincipal.getUsername());
  }

  public static String asJsonString(final Object obj) {
    try {
      return new ObjectMapper().writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
