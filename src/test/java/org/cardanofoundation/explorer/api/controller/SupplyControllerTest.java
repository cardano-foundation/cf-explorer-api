package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
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
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.service.SupplyService;

@WebMvcTest(SupplyController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class SupplyControllerTest {
  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private SupplyService supplyService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetSupplyCirculating() throws Exception {
    // Arrange
    long circulatingSupply = 32000000L;
    when(supplyService.getSupplyCirculating()).thenReturn(circulatingSupply);

    // Act & Assert
    mockMvc
        .perform(get("/api/v1/supply/circulating").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$", is(32000000)))
        .andExpect(jsonPath("$").exists());

    verify(supplyService).getSupplyCirculating();
  }

  @Test
  void testGetSupplyTotal() throws Exception {
    // Arrange
    long totalSupply = 45000000L;
    when(supplyService.getSupplyTotal()).thenReturn(totalSupply);

    // Act & Assert
    mockMvc
        .perform(get("/api/v1/supply/total").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$", is(45000000)))
        .andExpect(jsonPath("$").exists());

    verify(supplyService).getSupplyTotal();
  }
}
