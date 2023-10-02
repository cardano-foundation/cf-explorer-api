package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.PolicyService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(PolicyController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class PolicyControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private PolicyService policyService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    void testGetPolicyDetail_thenReturn() throws Exception {
        String policyId = "1";
        PolicyResponse policyResponse = PolicyResponse.builder().build();

        when(policyService.getPolicyDetail(policyId)).thenReturn(policyResponse);

        mockMvc.perform(get("/api/v1/policies/{policyId}", policyId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetTokens_thenReturn() throws Exception {
        String policyId = "1";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<TokenFilterResponse> response = new BaseFilterResponse<>(List.of(), 0);

        when(policyService.getTokens(policyId, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/policies/{policyId}/tokens", policyId)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }

    @Test
    void testGetHolders_thenReturn() throws Exception {
        String policyId = "1";
        Pageable pageable = PageRequest.of(0, 10);
        BaseFilterResponse<TokenAddressResponse> response = new BaseFilterResponse<>(List.of(), 0);

        when(policyService.getHolders(policyId, pageable)).thenReturn(response);

        mockMvc.perform(get("/api/v1/policies/{policyId}/holders", policyId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());
    }
}