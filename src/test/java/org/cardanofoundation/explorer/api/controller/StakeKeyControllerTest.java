package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.StakeAnalyticResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.StakeAddressResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeTxResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.service.StakeKeyService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@WebMvcTest(StakeKeyController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class StakeKeyControllerTest {
    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private StakeKeyService stakeService;

    @MockBean
    private TxService txService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    void testGetDataForStakeRegistration_thenReturn() throws Exception {
        Pageable pageable = PageRequest.of(0, 10, Sort.Direction.DESC, "txId");
        List<StakeTxResponse> mockResponse = Arrays.asList(
            new StakeTxResponse(),
            new StakeTxResponse()
        );
        BaseFilterResponse<StakeTxResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        when(stakeService.getDataForStakeKeyRegistration(pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/registration")
                        .param("page", "0")
                        .param("size", "10")
                        .param("sort", "txId,desc"))
            .andExpect(jsonPath("$.data").isArray())
            .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));
        verify(stakeService).getDataForStakeKeyRegistration(pageable);
    }

    @Test
    void testGetDataForStakeDeRegistration_thenReturn() throws Exception {
        Pageable pageable = PageRequest.of(0, 10, Sort.Direction.DESC, "txId");
        List<StakeTxResponse> mockResponse = Arrays.asList(
            new StakeTxResponse(),
            new StakeTxResponse()
        );
        BaseFilterResponse<StakeTxResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

        when(stakeService.getDataForStakeKeyDeRegistration(pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/de-registration")
                .param("page", "0")
                .param("size", "10")
                .param("sort", "txId,desc"))
            .andExpect(jsonPath("$.data").isArray())
            .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));
        verify(stakeService).getDataForStakeKeyDeRegistration(pageable);
    }

    @Test
    void testGetStakeDetailByAddress_thenReturn() throws Exception {
        String address = "address";
        StakeAddressResponse response = new StakeAddressResponse();

        when(stakeService.getStakeByAddress(address)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/address/{address}", address))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeDetail_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        StakeAddressResponse response = new StakeAddressResponse();

        when(stakeService.getStake(stakeKey)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}", stakeKey))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetTransactions_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();

        when(txService.getTransactionsByStake(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/txs", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetDelegationHistories_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<StakeDelegationProjection> response = new BaseFilterResponse<>();

        when(stakeService.getDelegationHistories(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/delegation-history", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeHistories_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<StakeHistoryProjection> response = new BaseFilterResponse<>();

        when(stakeService.getStakeHistories(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/stake-history", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetWithdrawalHistories_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<StakeWithdrawalProjection> response = new BaseFilterResponse<>();

        when(stakeService.getWithdrawalHistories(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/withdrawal-history", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetInstantaneousRewards_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<StakeInstantaneousRewardsProjection> response = new BaseFilterResponse<>();

        when(stakeService.getInstantaneousRewards(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/instantaneous-rewards", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetTopDelegators_thenReturn() throws Exception {
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<StakeFilterResponse> response = new BaseFilterResponse<>();

        when(stakeService.getTopDelegators(pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/top-delegators")
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetAddresses_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<AddressFilterResponse> response = new BaseFilterResponse<>();

        when(stakeService.getAddresses(stakeKey, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/{stakeKey}/list-address", stakeKey)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeAnalytics_thenReturn() throws Exception {
        StakeAnalyticResponse response = new StakeAnalyticResponse();

        when(stakeService.getStakeAnalytics()).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/analytics"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        AnalyticType type = AnalyticType.ONE_DAY;
        List<StakeAnalyticBalanceResponse> response = List.of();

        when(stakeService.getStakeBalanceAnalytics(stakeKey, type)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/analytics-balance/{stakeKey}/{type}", stakeKey, type))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeRewardAnalytics_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        List<StakeAnalyticRewardResponse> response = List.of();

        when(stakeService.getStakeRewardAnalytics(stakeKey)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/analytics-reward/{stakeKey}", stakeKey))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetStakeMinMaxBalance_thenReturn() throws Exception {
        String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
        List<BigInteger> response = List.of();

        when(stakeService.getStakeMinMaxBalance(stakeKey)).thenReturn(response);

        mockMvc.perform(get("/api/v1/stakes/min-max-balance/{stakeKey}", stakeKey))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }
}
