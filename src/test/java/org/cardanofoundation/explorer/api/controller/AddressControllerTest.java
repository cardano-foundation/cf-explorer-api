package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.web.servlet.MockMvc;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceData;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.service.TxService;

@WebMvcTest(AddressController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class AddressControllerTest {
  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private AddressService addressService;

  @MockBean private TxService txService;

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
    AddressResponse mockResponse = AddressResponse.builder().address("addr123456789").build();

    // Mock the service method
    when(addressService.getAddressDetail(address)).thenReturn(mockResponse);

    // Perform the GET request
    mockMvc
        .perform(get("/api/v1/addresses/{address}", address))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.address").exists());

    // Verify that the service method was called with the correct argument
    verify(addressService).getAddressDetail(address);
  }

  @Test
  public void testGetAddressAnalytics() throws Exception {
    // Mock request and response objects
    String address = "testAddress";
    AnalyticType type = AnalyticType.ONE_DAY;
    List<AddressChartBalanceData> data =
        Arrays.asList(new AddressChartBalanceData(), new AddressChartBalanceData());

    AddressChartBalanceResponse mockResponse =
        AddressChartBalanceResponse.builder()
            .data(data)
            .highestBalance(BigInteger.valueOf(100))
            .lowestBalance(BigInteger.valueOf(50))
            .build();

    // Mock the service method
    when(addressService.getAddressAnalytics(address, type)).thenReturn(mockResponse);

    // Perform the GET request
    mockMvc
        .perform(get("/api/v1/addresses/analytics/{address}/{type}", address, type))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data.length()").value(mockResponse.getData().size()));

    // Verify that the service method was called with the correct arguments
    verify(addressService).getAddressAnalytics(address, type);
  }

  //  @Test
  //  public void testGetTransactions() throws Exception {
  //    // Mock request and response objects
  //    String address = "testAddress";
  //    List<TxFilterResponse> mockResponse =
  //        Arrays.asList(new TxFilterResponse(), new TxFilterResponse());
  //    BaseFilterResponse<TxFilterResponse> response =
  //        new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());
  //
  //    // Mock the service method
  //    when(txService.getTransactionsByAddress(address, pageable)).thenReturn(response);
  //
  //    // Perform the GET request
  //    mockMvc
  //        .perform(
  //            get("/api/v1/addresses/{address}/txs", address).param("page", "0").param("size",
  // "10"))
  //        .andExpect(status().isOk())
  //        .andExpect(jsonPath("$.data").isArray())
  //        .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));
  //
  //    // Verify that the service method was called with the correct arguments
  //    verify(txService).getTransactionsByAddress(address, pageable);
  //  }

  @Test
  public void testGetTokenByAddress() throws Exception {
    // Mock request and response objects
    String address = "testAddress";
    String displayName = "testToken";
    Pageable pageable = PageRequest.of(0, 10);
    List<TokenAddressResponse> mockResponse =
        Arrays.asList(new TokenAddressResponse(), new TokenAddressResponse());
    BaseFilterResponse<TokenAddressResponse> response =
        new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());

    // Mock the service method
    when(addressService.getTokenByDisplayName(pageable, address, displayName)).thenReturn(response);

    // Perform the GET request
    mockMvc
        .perform(
            get("/api/v1/addresses/{address}/tokens", address)
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
