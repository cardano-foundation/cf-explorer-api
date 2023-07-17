package org.cardanofoundation.explorer.api.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.request.pool.report.PoolReportCreateRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportExportResponse;
import org.cardanofoundation.explorer.api.model.response.pool.report.PoolReportListResponse;
import org.cardanofoundation.explorer.api.service.PoolReportService;
import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

@WebMvcTest(PoolReportController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class PoolReportControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private PoolReportService poolReportService;

    @MockBean
    private AuthInterceptor authInterceptor;

    private ObjectMapper objectMapper;

    private PageRequest pageable;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
        this.objectMapper = new ObjectMapper();
        this.pageable = PageRequest.of(0, 10);
    }

    @Test
    public void testCreatePoolReport() throws Exception {
        // Mock request and response objects
        PoolReportCreateRequest request = new PoolReportCreateRequest();
        String username = "testUser";

        // Mock the service method
        when(poolReportService.create(request, username)).thenReturn(true);

        // Perform the POST request
        mockMvc.perform(post("/api/v1/pool-report/create")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(request))
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(content().string("true"));

        verify(poolReportService).create(request, username);
    }

    @Test
    public void testExportPoolReport() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        ExportType exportType = ExportType.EXCEL;
        String username = "testUser";

        // Mock the service method
        PoolReportExportResponse response = PoolReportExportResponse.builder()
                .fileName("filename.xlsx")
                .byteArrayInputStream(new ByteArrayInputStream(new byte[]{}))
                .build();
        when(poolReportService.export(reportId, exportType, username)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/export", reportId)
                        .param("exportType", exportType.name())
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.fileName").doesNotExist());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).export(reportId, exportType, username);
    }

    @Test
    public void testListPoolReport() throws Exception {
        // Mock request and response objects

        String username = "testUser";
        List<PoolReportListResponse> mockResponse = Arrays.asList(
                new PoolReportListResponse(),
                new PoolReportListResponse()
        );
        BaseFilterResponse<PoolReportListResponse> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.list(any(), any(), any())).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/list")
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

    }

    @Test
    public void testDetailEpochSizePoolReport() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        List<PoolReportDetailResponse.EpochSize> mockResponse = Arrays.asList(
                new PoolReportDetailResponse.EpochSize(),
                new PoolReportDetailResponse.EpochSize()
        );
        BaseFilterResponse<PoolReportDetailResponse.EpochSize> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.fetchEpochSize(reportId, pageable, username)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/epoch-size", reportId)
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).fetchEpochSize(reportId, pageable, username);
    }

    @Test
    public void testDetailPoolRegistration() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        BaseFilterResponse<TabularRegisResponse> response = new BaseFilterResponse<>(new ArrayList<>(), this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.fetchPoolRegistration(reportId, pageable, username)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/pool-registration", reportId)
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).fetchPoolRegistration(reportId, pageable, username);
    }

    @Test
    public void testDetailPoolUpdate() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        BaseFilterResponse<PoolUpdateDetailResponse> response = new BaseFilterResponse<>(new ArrayList<>(), this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.fetchPoolUpdate(reportId, pageable, username)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/pool-update", reportId)
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).fetchPoolUpdate(reportId, pageable, username);
    }

    @Test
    public void testDetailRewardsDistribution() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        BaseFilterResponse<RewardResponse> response = new BaseFilterResponse<>(new ArrayList<>(), this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.fetchRewardsDistribution(reportId, pageable, username)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/rewards-distribution", reportId)
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).fetchRewardsDistribution(reportId, pageable, username);
    }

    @Test
    public void testDetailDeregistration() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        DeRegistrationResponse deRegistrationResponse = DeRegistrationResponse.builder()
                .poolId(reportId.toString())
                .poolName("name")
                .build();
        BaseFilterResponse<DeRegistrationResponse> response = new BaseFilterResponse<>(List.of(deRegistrationResponse), this.pageable.getPageSize());

        // Mock the service method
        when(poolReportService.fetchDeregistraion(any(), any(), any())).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}/deregistration", reportId)
                        .param("page", "0")
                        .param("size", "10")
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").exists());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).fetchDeregistraion(reportId, pageable, username);
    }

    @Test
    public void testDetailPoolReport() throws Exception {
        // Mock request and response objects
        Long reportId = 1L;
        String username = "testUser";
        PoolReportHistory mockResponse = PoolReportHistory.builder()
                .id(1L)
                .poolView("poolview12345678")
                .build();

        // Mock the service method
        when(poolReportService.detail(reportId, username)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/pool-report/detail/{reportId}", reportId)
                        .header("username", username)
                        .with(t -> {
                            t.setAttribute("username", username);
                            return t;
                        }))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").exists());

        // Verify that the service method was called with the correct arguments
        verify(poolReportService).detail(reportId, username);
    }

}