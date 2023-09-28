
package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.security.configuration.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.*;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.mockito.Mockito;
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

import java.math.BigInteger;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(EpochController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class EpochControllerTest {

    @MockBean
    private AuthInterceptor authInterceptor;

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private EpochService epochService;

    @MockBean
    private BlockService blockService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    public void testGetEpochDetail() throws Exception {
        String epochNo = "1";
        EpochResponse expectedResponse = EpochResponse.builder().blkCount(1).txCount(1).maxSlot(1).no(1).outSum(BigInteger.ONE).build();

        // Mock the service method
        Mockito.when(epochService.getEpochDetail(epochNo)).thenReturn(expectedResponse);

        mockMvc.perform(get("/api/v1/epochs/{no}", epochNo))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists()) // Validate if data exists in the response
                // Add more assertions as needed based on your expected response
                .andReturn();
    }

    @Test
    public void testGetBlockList() throws Exception {
        String epochNo = "1";
        Pageable pageable = PageRequest.of(1, 20, Sort.Direction.DESC, "id");

        BaseFilterResponse<BlockFilterResponse> expectedResponse = new BaseFilterResponse<>(); // Set your expected response here

        // Mock the service method
        Mockito.when(blockService.getBlockByEpoch(epochNo, pageable)).thenReturn(expectedResponse);

        mockMvc.perform(get("/api/v1/epochs/{no}/blocks", epochNo)
                        .param("page", "1")
                        .param("size", "20")
                        .param("sort", "id,desc"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists()) // Validate if data exists in the response
                // Add more assertions as needed based on your expected response
                .andReturn();
    }

    @Test
    public void testFilter() throws Exception {
        Pageable pageable = PageRequest.of(1, 20, Sort.Direction.DESC, "id");

        BaseFilterResponse<EpochResponse> expectedResponse = new BaseFilterResponse<>(); // Set your expected response here

        // Mock the service method
        Mockito.when(epochService.getAllEpoch(pageable)).thenReturn(expectedResponse);

        mockMvc.perform(get("/api/v1/epochs")
                        .param("page", "1")
                        .param("size", "20")
                        .param("sort", "id,desc"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists()) // Validate if data exists in the response
                // Add more assertions as needed based on your expected response
                .andReturn();
    }

    @Test
    public void testFindCurrentEpoch() throws Exception {
        EpochSummary expectedResponse = new EpochSummary(); // Set your expected response here

        // Mock the service method
        Mockito.when(epochService.getCurrentEpochSummary()).thenReturn(expectedResponse);

        mockMvc.perform(get("/api/v1/epochs/current"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists()) // Validate if data exists in the response
                // Add more assertions as needed based on your expected response
                .andReturn();
    }
}