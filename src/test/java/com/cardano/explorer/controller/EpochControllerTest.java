package com.cardano.explorer.controller;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.EpochResponse;
import com.cardano.explorer.service.EpochService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@WebMvcTest(EpochController.class)
class EpochControllerTest {
  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private EpochService epochService;


  @Test
  void shouldGetCurrentEpoch() throws Exception {
    given(epochService.getCurrentEpoch()).willReturn(200);
    mockMvc.perform(get("/api/v1/epoch/current").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }

  @Test
  void shouldGetEpochDetail() throws Exception {
    given(epochService.getEpochDetail(20)).willReturn(new EpochResponse());
    mockMvc.perform(get("/api/v1/epoch/20").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }

  @Test
  void shouldGetAllEpoch() throws Exception {
    given(epochService.filterEpoch(PageRequest.of(0, 20))).willReturn(new BaseFilterResponse<>());
    mockMvc.perform(get("/api/v1/epoch/list?page=0&size=20").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }
}
