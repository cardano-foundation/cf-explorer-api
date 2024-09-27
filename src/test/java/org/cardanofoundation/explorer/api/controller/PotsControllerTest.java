package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;

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
import org.cardanofoundation.explorer.api.model.response.PotsOverviewResponse;
import org.cardanofoundation.explorer.api.service.PotsService;

@WebMvcTest(PotsController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class PotsControllerTest {
  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private PotsService potsService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetPotsOverview() throws Exception {
    // Setup
    final PotsOverviewResponse response =
        PotsOverviewResponse.builder()
            .depositsAndFees(BigInteger.ONE)
            .rewards(BigInteger.TEN)
            .treasury(BigInteger.ZERO)
            .reserves(BigInteger.ZERO)
            .epoch(200)
            .build();
    when(potsService.getPotsOverview()).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/pots/overview").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.depositsAndFees").value(BigInteger.ONE))
        .andExpect(jsonPath("$.rewards").value(BigInteger.TEN))
        .andExpect(jsonPath("$.treasury").value(BigInteger.ZERO))
        .andExpect(jsonPath("$.reserves").value(BigInteger.ZERO))
        .andExpect(jsonPath("$.epoch").value(200));

    verify(potsService).getPotsOverview();
  }
}
