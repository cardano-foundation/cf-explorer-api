package org.cardanofoundation.explorer.api.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.healthcheck.SyncStatus;
import org.cardanofoundation.explorer.api.service.HealthCheckService;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@WebMvcTest(HealthCheckController.class)
@Import({
    SpringWebSecurityConfig.class,
    WebConfig.class,
    JacksonMapperDateConfig.class,
    GlobalRestControllerExceptionHandler.class,
    RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
class HealthCheckControllerTest {

  @MockBean
  private AuthInterceptor authInterceptor;

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private HealthCheckService mockHealthCheckService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testCheckSyncStatus_WhenSyncOK() throws Exception {

    final SyncStatus syncStatus = SyncStatus.builder()
        .isSyncing(Boolean.TRUE).build();
    when(mockHealthCheckService.getSyncStatus()).thenReturn(syncStatus);

    final MockHttpServletResponse response = mockMvc.perform(get("/api/v1/health-check/sync-status")
            .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(syncStatus), response.getContentAsString());
  }

  @Test
  void testCheckSyncStatus_WhenSyncNotOK() throws Exception {

    final SyncStatus syncStatus = SyncStatus.builder()
        .isSyncing(Boolean.FALSE).build();
    when(mockHealthCheckService.getSyncStatus()).thenReturn(syncStatus);

    final MockHttpServletResponse response = mockMvc.perform(get("/api/v1/health-check/sync-status")
            .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), response.getStatus());
    assertEquals(asJsonString(syncStatus), response.getContentAsString());
  }

  public static String asJsonString(final Object obj) {
    try {
      return new ObjectMapper().writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
