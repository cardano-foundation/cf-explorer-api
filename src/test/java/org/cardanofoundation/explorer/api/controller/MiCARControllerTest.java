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
import org.springframework.data.domain.PageRequest;
import org.springframework.test.web.servlet.MockMvc;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.micar.AddressCarbonEmissionResponse;
import org.cardanofoundation.explorer.api.service.MiCARService;

@WebMvcTest(MiCARController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class MiCARControllerTest {

  @MockBean private AuthInterceptor authInterceptor;

  private PageRequest pageable;

  @Autowired private MockMvc mockMvc;

  @MockBean private MiCARService miCARService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    this.pageable = PageRequest.of(0, 10);
  }

  @Test
  void testCalculateCarbonEmission_ByStakeAddressAndAddress() throws Exception {
    String query =
        "addr1q8elqhkuvtyelgcedpup58r893awhg3l87a4rz5d5acatuj9y84nruafrmta2rewd5l46g8zxy4l49ly8kye79ddr3ksqal35g";
    AddressCarbonEmissionResponse result =
        AddressCarbonEmissionResponse.builder()
            .address(query)
            .txCount(20L)
            .carbonEmissionPerTx(0.000018987)
            .build();

    when(miCARService.getCarbonEmissionsByAddressAndPool(anyString())).thenReturn(result);

    mockMvc
        .perform(get("/api/v1/MiCAR/carbon-emission/{address}", query))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.address").value(query))
        .andExpect(jsonPath("$.txCount").value(20))
        .andExpect(jsonPath("$.carbonEmissionPerTx").value(0.000018987))
        .andExpect(jsonPath("$.stakeAddress").isEmpty());

    verify(miCARService).getCarbonEmissionsByAddressAndPool(anyString());
  }
}
