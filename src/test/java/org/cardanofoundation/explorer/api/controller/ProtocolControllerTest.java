package org.cardanofoundation.explorer.api.controller;

import io.jsonwebtoken.lang.Strings;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.mockito.Mockito.verify;

@WebMvcTest(ProtocolParamController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class ProtocolControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private ProtocolParamService protocolParamService;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    }

    @Test
    void testGetCurrentProtocolWithFilter() throws Exception {
        // Mocking the service response
        List<ProtocolType> protocolTypes = Arrays.asList(ProtocolType.MIN_FEE_A, ProtocolType.MIN_FEE_B);
        BigInteger startTime = BigInteger.valueOf(123456);
        BigInteger endTime = BigInteger.valueOf(789012);
        HistoriesProtocol mockHistoriesProtocol = new HistoriesProtocol(); // Create your mock response here
        when(protocolParamService.getHistoryProtocolParameters(protocolTypes, startTime, endTime))
                .thenReturn(mockHistoriesProtocol);

        // Perform the GET request and validate the response
        mockMvc.perform(get("/api/v1/protocols/histories/filter/{protocolsTypes}", Strings.collectionToCommaDelimitedString(List.of(ProtocolType.MIN_FEE_A, ProtocolType.MIN_FEE_B)))
                        .param("startTime", "123456")
                        .param("endTime", "789012"))
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called with the correct arguments
        verify(protocolParamService).getHistoryProtocolParameters(protocolTypes, startTime, endTime);
    }

    @Test
    void testGetLatestChange() throws Exception {
        // Mocking the service response
        Protocols mockProtocols = new Protocols(); // Create your mock response here
        when(protocolParamService.getLatestChange()).thenReturn(mockProtocols);

        // Perform the GET request and validate the response
        mockMvc.perform(get("/api/v1/protocols/latest"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called
        verify(protocolParamService).getLatestChange();
    }

    @Test
    void testGetFixedProtocols() throws Exception {
        // Mocking the service response
        FixedProtocol mockFixedProtocol = new FixedProtocol(); // Create your mock response here
        when(protocolParamService.getFixedProtocols()).thenReturn(mockFixedProtocol);

        // Perform the GET request and validate the response
        mockMvc.perform(get("/api/v1/protocols/fixed"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").exists());

        // Verify that the service method was called
        verify(protocolParamService).getFixedProtocols();
    }
}