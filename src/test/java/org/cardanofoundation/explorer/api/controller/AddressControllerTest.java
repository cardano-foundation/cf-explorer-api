package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.security.configuration.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(AddressController.class)
@Import({
        SpringWebSecurityConfig.class,
        WebConfig.class,
        JacksonMapperDateConfig.class,
        GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class AddressControllerTest {
    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private AuthInterceptor authInterceptor;

    @MockBean
    private AddressService addressService;

    @MockBean
    private TxService txService;

    private PageRequest pageable;

    @BeforeEach
    void preControllerTest() throws Exception {
        when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
        this.pageable = PageRequest.of(0, 10);
    }

    @Test
    public void testGetAddressDetail() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        AddressResponse mockResponse = AddressResponse.builder()
                .address("addr123456789")
                .build();

        // Mock the service method
        when(addressService.getAddressDetail(address)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/{address}", address))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.address").exists());

        // Verify that the service method was called with the correct argument
        verify(addressService).getAddressDetail(address);
    }

    @Test
    public void testGetTopAddress() throws Exception {
        // Mock request and response objects
        List<AddressFilterResponse> mockResponse = Arrays.asList(
                new AddressFilterResponse(),
                new AddressFilterResponse()
        );
        BaseFilterResponse<AddressFilterResponse> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(addressService.getTopAddress(pageable)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/top-addresses")
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct argument
        verify(addressService).getTopAddress(pageable);
    }

    @Test
    public void testGetAddressAnalytics() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        AnalyticType type = AnalyticType.ONE_DAY;
        List<AddressAnalyticsResponse> mockResponse = Arrays.asList(
                new AddressAnalyticsResponse(),
                new AddressAnalyticsResponse()
        );

        // Mock the service method
        when(addressService.getAddressAnalytics(address, type)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/analytics/{address}/{type}", address, type))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(addressService).getAddressAnalytics(address, type);
    }

    @Test
    public void testGetAddressMinMaxBalance() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        List<BigInteger> mockResponse = Arrays.asList(
                BigInteger.valueOf(100),
                BigInteger.valueOf(200)
        );

        // Mock the service method
        when(addressService.getAddressMinMaxBalance(address)).thenReturn(mockResponse);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/min-max-balance/{address}", address))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct argument
        verify(addressService).getAddressMinMaxBalance(address);
    }

    @Test
    public void testGetTransactions() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        List<TxFilterResponse> mockResponse = Arrays.asList(
                new TxFilterResponse(),
                new TxFilterResponse()
        );
        BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(txService.getTransactionsByAddress(address, pageable)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/{address}/txs", address)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(txService).getTransactionsByAddress(address, pageable);
    }

    @Test
    public void testGetTokenByAddress() throws Exception {
        // Mock request and response objects
        String address = "testAddress";
        String displayName = "testToken";
        Pageable pageable = PageRequest.of(0, 10);
        List<TokenAddressResponse> mockResponse = Arrays.asList(
                new TokenAddressResponse(),
                new TokenAddressResponse()
        );
        BaseFilterResponse<TokenAddressResponse> response = new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

        // Mock the service method
        when(addressService.getTokenByDisplayName(pageable, address, displayName)).thenReturn(response);

        // Perform the GET request
        mockMvc.perform(get("/api/v1/addresses/{address}/tokens", address)
                        .param("displayName", displayName)
                        .param("page", "0")
                        .param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

        // Verify that the service method was called with the correct arguments
        verify(addressService).getTokenByDisplayName(pageable, address, displayName);
    }

    // Write additional tests for other methods in AddressController

}