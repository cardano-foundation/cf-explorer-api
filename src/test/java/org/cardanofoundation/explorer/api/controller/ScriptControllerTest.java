package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.PolicyResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.PolicyService;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
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

@WebMvcTest(ScriptController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class,
        RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class ScriptControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private ScriptService scriptService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    void testGetSmartContracts_thenReturn() throws Exception {
        SmartContractFilterResponse smartContractFilterResponse = SmartContractFilterResponse.builder()
            .associatedAddress(List.of("addr1"))
            .scriptHash("hash")
            .version(ScriptType.PLUTUSV1)
            .build();
        BaseFilterResponse<SmartContractFilterResponse> response =
            new BaseFilterResponse<>(List.of(smartContractFilterResponse), 1);

        when(scriptService.getSmartContracts(any(Pageable.class))).thenReturn(response);

        mockMvc.perform(get("/api/v1/scripts/smart-contracts"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$").exists())
            .andExpect(jsonPath("$.data[0].associatedAddress[0]").value("addr1"))
            .andExpect(jsonPath("$.data[0].scriptHash").value("hash"))
            .andExpect(jsonPath("$.data[0].version").value("PLUTUSV1"));
    }

    @Test
    void testNativeScripts_thenReturn() throws Exception {
        NativeScriptFilterResponse nativeScriptFilterResponse = NativeScriptFilterResponse.builder()
            .scriptHash("hash")
            .numberOfTokens(1)
            .numberOfAssetHolders(1)
            .build();
        BaseFilterResponse<NativeScriptFilterResponse> response =
            new BaseFilterResponse<>(List.of(nativeScriptFilterResponse), 1);

        when(scriptService.getNativeScripts(any(Pageable.class))).thenReturn(response);

        mockMvc.perform(get("/api/v1/scripts/native-scripts"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$").exists())
            .andExpect(jsonPath("$.data[0].numberOfTokens").value("1"))
            .andExpect(jsonPath("$.data[0].numberOfAssetHolders").value("1"))
            .andExpect(jsonPath("$.data[0].scriptHash").value("hash"));
    }
}