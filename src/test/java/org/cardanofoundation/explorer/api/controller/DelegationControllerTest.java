package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.PoolDetailDelegatorResponse;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.service.DelegationService;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(DelegationController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class DelegationControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private DelegationService delegationService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    public void testGetDelegations() throws Exception {
        // Mock request and response objects
        Pageable pageable = PageRequest.of(0, 20, Sort.Direction.DESC, "txId");
        List<DelegationResponse> mockResponse = Arrays.asList(
                new DelegationResponse(),
                new DelegationResponse()
        );
        BaseFilterResponse<DelegationResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        // Mock the service method
        when(delegationService.getDelegations(any())).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations")
                        .param("page", "0")
                        .param("size", "20"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct argument
        verify(delegationService).getDelegations(pageable);
    }

    @Test
    public void testGetDataForDelegationHeader() throws Exception {
        // Mock response object
        DelegationHeaderResponse mockResponse = new DelegationHeaderResponse();

        // Mock the service method
        when(delegationService.getDataForDelegationHeader()).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/header"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called
        verify(delegationService).getDataForDelegationHeader();
    }

    @Test
    public void testGetDataForPoolTable() throws Exception {
        // Mock request and response objects
        Pageable pageable = PageRequest.of(0, 10);
        String search = "testSearch";
        List<PoolResponse> mockResponse = Arrays.asList(
                new PoolResponse(),
                new PoolResponse()
        );
        BaseFilterResponse<PoolResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        // Mock the service method
        when(delegationService.getDataForPoolTable(pageable, search)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/pool-list")
                        .param("search", search)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(delegationService).getDataForPoolTable(pageable, search);
    }

    @Test
    public void testGetDataForPoolDetail() throws Exception {
        // Mock request and response objects
        String poolView = "pool1m06tlj2ykawzvweacgmhxj43hykczgfuynk2lqzxvshm5lq2lyq";
        PoolDetailHeaderResponse mockResponse = new PoolDetailHeaderResponse();

        // Mock the service method
        when(delegationService.getDataForPoolDetail(poolView)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/pool-detail-header/{poolView}", poolView))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called with the correct argument
        verify(delegationService).getDataForPoolDetail(poolView);
    }

    @Test
    public void testGetAnalyticsForPoolDetail() throws Exception {
        // Mock request and response objects
        String poolView = "pool1m06tlj2ykawzvweacgmhxj43hykczgfuynk2lqzxvshm5lq2lyq";
        PoolDetailAnalyticsResponse mockResponse = new PoolDetailAnalyticsResponse();

        // Mock the service method
        when(delegationService.getAnalyticsForPoolDetail(poolView)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/pool-detail-analytics")
                        .param("poolView", poolView))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called with the correct argument
        verify(delegationService).getAnalyticsForPoolDetail(poolView);
    }

    @Test
    public void testGetEpochListForPoolDetail() throws Exception {
        // Mock request and response objects
        String poolView = "pool1m06tlj2ykawzvweacgmhxj43hykczgfuynk2lqzxvshm5lq2lyq";
        Pageable pageable = PageRequest.of(0, 10);
        List<PoolDetailEpochResponse> mockResponse = Arrays.asList(
                new PoolDetailEpochResponse(),
                new PoolDetailEpochResponse()
        );
        BaseFilterResponse<PoolDetailEpochResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        // Mock the service method
        when(delegationService.getEpochListForPoolDetail(pageable, poolView)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/pool-detail-epochs")
                        .param("poolView", poolView)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(delegationService).getEpochListForPoolDetail(pageable, poolView);
    }

    @Test
    public void testGetDelegatorForPoolDetail() throws Exception {
        // Mock request and response objects
        String poolView = "pool1m06tlj2ykawzvweacgmhxj43hykczgfuynk2lqzxvshm5lq2lyq";
        Pageable pageable = PageRequest.of(0, 10);
        List<PoolDetailDelegatorResponse> mockResponse = Arrays.asList(
                new PoolDetailDelegatorResponse(),
                new PoolDetailDelegatorResponse()
        );
        BaseFilterResponse<PoolDetailDelegatorResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        // Mock the service method
        when(delegationService.getDelegatorsForPoolDetail(pageable, poolView)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/pool-detail-delegators")
                        .param("poolView", poolView)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(delegationService).getDelegatorsForPoolDetail(pageable, poolView);
    }

    @Test
    public void testFindTopDelegationPool() throws Exception {
        // Mock request and response objects
        Pageable pageable = PageRequest.of(0, 3);
        List<PoolResponse> mockResponse = Arrays.asList(
                new PoolResponse(),
                new PoolResponse(),
                new PoolResponse()
        );

        // Mock the service method
        when(delegationService.findTopDelegationPool(pageable)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/delegations/top")
                        .param("page", "0")
                        .param("size", "3"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct argument
        verify(delegationService).findTopDelegationPool(pageable);
    }

}