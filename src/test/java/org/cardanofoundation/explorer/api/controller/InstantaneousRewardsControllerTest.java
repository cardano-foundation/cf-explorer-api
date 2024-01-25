package org.cardanofoundation.explorer.api.controller;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.web.servlet.MockMvc;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.InstantaneousRewardsResponse;
import org.cardanofoundation.explorer.api.service.InstantaneousRewardsService;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@WebMvcTest(InstantaneousRewardsController.class)
@Import({
    SpringWebSecurityConfig.class,
    WebConfig.class,
    JacksonMapperDateConfig.class,
    GlobalRestControllerExceptionHandler.class,
    RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class InstantaneousRewardsControllerTest {
  @MockBean
  private AuthInterceptor authInterceptor;

  @Autowired
  private MockMvc mockMvc;

  private PageRequest pageable;

  @MockBean
  private InstantaneousRewardsService instantaneousRewardsService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
    this.pageable = PageRequest.of(0,10);
  }

  @Test
  public void testGetInstantaneousRewards() throws Exception{
    List<InstantaneousRewardsResponse> mockResponse = Arrays.asList(
        new InstantaneousRewardsResponse(),
        new InstantaneousRewardsResponse()
    );
    BaseFilterResponse<InstantaneousRewardsResponse> response = new BaseFilterResponse<>(mockResponse,this.pageable.getPageSize());

    when(instantaneousRewardsService.getAll(this.pageable)).thenReturn(response);

    mockMvc.perform(get("/api/v1/instantaneous-rewards")
        .param("page","0")
        .param("size","10"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data").isArray())
        .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

    verify(instantaneousRewardsService).getAll(pageable);
  }
}
