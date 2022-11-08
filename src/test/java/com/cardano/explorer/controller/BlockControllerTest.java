package com.cardano.explorer.controller;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.BlockResponse;
import com.cardano.explorer.service.BlockService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@WebMvcTest(BlockController.class)
class BlockControllerTest {
  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private BlockService blockService;


  @Test
  void shouldGetBlockDetail() throws Exception {
    given(blockService.getBlockDetail(1)).willReturn(new BlockResponse());
    mockMvc.perform(get("/api/v1/block/1").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }

  @Test
  void shouldGetBlockList() throws Exception {
    given(blockService.getAllBlock(PageRequest.of(0, 20))).willReturn(new BaseFilterResponse<>());
    mockMvc.perform(get("/api/v1/block/list?page=0&size=20").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }

  @Test
  void shouldGetBlockByEpochNo() throws Exception {
    given(blockService.getBlockByEpochNo(200, PageRequest.of(0, 20))).willReturn(new BaseFilterResponse<>());
    mockMvc.perform(get("/api/v1/block?page=0&size=20&epochNo=200").contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andDo(print());
  }
}
