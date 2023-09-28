package org.cardanofoundation.explorer.api.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.security.configuration.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractScript;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

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

@WebMvcTest(ContractController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class ContractControllerTest {
    @MockBean
    private AuthInterceptor authInterceptor;

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AddressService addressService;

    private PageRequest pageable;

    private ObjectMapper objectMapper;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
        this.pageable = PageRequest.of(0, 20);
        this.objectMapper = new ObjectMapper();
    }

    @Test
    public void testFilterContract() throws Exception {
        // Mock request and response objects
        List<ContractFilterResponse> mockResponse = Arrays.asList(
                new ContractFilterResponse(),
                new ContractFilterResponse()
        );
        BaseFilterResponse<ContractFilterResponse> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(addressService.getContracts(any())).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/contracts")
                        .param("page", "0")
                        .param("size", "20"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct argument
        verify(addressService).getContracts(PageRequest.of(this.pageable.getPageNumber(), this.pageable.getPageSize(), Sort.Direction.DESC, "balance"));
    }

    @Test
    public void testVerifyContract() throws Exception {
        // Mock request and response objects
        ScriptVerifyRequest mockRequest = new ScriptVerifyRequest();
        boolean mockResponse = true;

        // Mock the service method
        when(addressService.verifyNativeScript(mockRequest)).thenReturn(mockResponse);
        when(addressService.verifyNativeScript(any())).thenReturn(mockResponse);

        // Perform the POST request
        mockMvc.perform(post("/api/v1/contracts/verify/native")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(mockRequest)))
                .andExpect(status().isOk())
                .andExpect(content().string(String.valueOf(mockResponse)));
    }

    @Test
    public void testGetScriptOfContract() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        String mockResponse = String.valueOf("testScript");
        ContractScript response = ContractScript.builder().isVerified(true).data(mockResponse).build();

        // Mock the service method
        when(addressService.getJsonNativeScript(address)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/contracts/{address}/script", address))
                .andExpect(status().isOk())
                .andExpect(content().json(new ObjectMapper().writeValueAsString(response)));

        // Verify that the service method was called with the correct argument
        verify(addressService).getJsonNativeScript(address);
    }

}