package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.BeforeEach;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.service.AddressService;

@WebMvcTest(ContractController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class ContractControllerTest {
  @MockBean private AuthInterceptor authInterceptor;

  @Autowired private MockMvc mockMvc;

  @MockBean private AddressService addressService;

  private PageRequest pageable;

  private ObjectMapper objectMapper;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    this.pageable = PageRequest.of(0, 20);
    this.objectMapper = new ObjectMapper();
  }

  //  @Test
  //  public void testFilterContract() throws Exception {
  //    // Mock request and response objects
  //    List<ContractFilterResponse> mockResponse =
  //        Arrays.asList(new ContractFilterResponse(), new ContractFilterResponse());
  //    BaseFilterResponse<ContractFilterResponse> response =
  //        new BaseFilterResponse<>(mockResponse, this.pageable.getPageSize());
  //
  //    // Mock the service method
  //    when(addressService.getContracts(any())).thenReturn(response);
  //
  //    // Perform the GET request
  //    mockMvc
  //        .perform(get("/api/v1/contracts").param("page", "0").param("size", "20"))
  //        .andExpect(status().isOk())
  //        .andExpect(jsonPath("$.data").isArray())
  //        .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));
  //
  //    // Verify that the service method was called with the correct argument
  //    verify(addressService)
  //        .getContracts(
  //            PageRequest.of(
  //                this.pageable.getPageNumber(),
  //                this.pageable.getPageSize(),
  //                Sort.Direction.DESC,
  //                "balance"));
  //  }
}
