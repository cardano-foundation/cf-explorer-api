package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.search.AddressSearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.SearchStakingLifecycle;
import org.cardanofoundation.explorer.api.service.SearchService;

@WebMvcTest(SearchController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class SearchControllerTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private SearchService searchService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testSearch() throws Exception {
    String query = "voluptate consequat ad laborum est";
    SearchResponse response = new SearchResponse();
    response.setValidPoolName(true);
    response.setValidTokenName(false);
    response.setTx("tx");
    response.setBlock("block");
    response.setAddress(new AddressSearchResponse("address", null, false, true));

    when(searchService.search(anyString())).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/search").param("query", query))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.tx").value("tx"))
        .andExpect(jsonPath("$.address.stakeAddress").value(true));

    verify(searchService).search(anyString());
  }

  @Test
  void testSearchStakingLifecycle() throws Exception {
    String query = "Ae2tdPwUPEZFSi1cTyL1ZL6bgixhc2vSy5heg6Zg9uP7PpumkAJ82Qprt8b";
    SearchStakingLifecycle response = new SearchStakingLifecycle();
    response.setAddress(
        new AddressSearchResponse(
            "Ae2tdPwUPEZFSi1cTyL1ZL6bgixhc2vSy5heg6Zg9uP7PpumkAJ82Qprt8b", null, true, false));

    when(searchService.searchForStakingLifecycle(anyString(), any())).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/search/staking-lifecycle").param("query", query))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.address.stakeAddressView").isEmpty())
        .andExpect(jsonPath("$.address.stakeAddress").value(false))
        .andExpect(jsonPath("$.address.address").value(query))
        .andExpect(jsonPath("$.address.paymentAddress").value(true));

    verify(searchService).searchForStakingLifecycle(anyString(), any());
  }
}
