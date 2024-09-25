package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;

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
import org.cardanofoundation.explorer.api.model.response.BolnisiProjectNumberResponse;
import org.cardanofoundation.explorer.api.service.BolnisiMetadataService;

@WebMvcTest(BolnisiController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class BolnisiControllerTest {
  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean BolnisiMetadataService bolnisiMetadataService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetBolnisiProjectNumber() throws Exception {
    BolnisiProjectNumberResponse response = new BolnisiProjectNumberResponse();
    response.setNumberOfBottles(BigInteger.valueOf(71683));
    response.setNumberOfWineries(BigInteger.valueOf(3));
    response.setNumberOfCertificates(BigInteger.ONE);

    when(bolnisiMetadataService.getBolnisiProjectNumber()).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/bolnisi/overview"))
        .andExpect(status().isOk())
        .andExpect(
            result -> {
              String content = result.getResponse().getContentAsString();
              assert content.contains("71683");
              assert content.contains("3");
              assert content.contains("1");
            });
  }
}
