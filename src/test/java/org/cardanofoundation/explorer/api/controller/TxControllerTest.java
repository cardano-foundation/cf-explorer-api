package org.cardanofoundation.explorer.api.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.model.response.tx.CollateralResponse;
import org.cardanofoundation.explorer.api.model.response.tx.SummaryResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxInfoResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.model.response.tx.UTxOResponse;
import org.cardanofoundation.explorer.api.service.BolnisiMetadataService;
import org.cardanofoundation.explorer.api.service.TxService;

@WebMvcTest(TxController.class)
@Import(RoleFilterMapper.class)
@AutoConfigureMockMvc(addFilters = false)
class TxControllerTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private TxService mockTxService;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private BolnisiMetadataService bolnisiMetadataService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  private static final String DATE_TIME_FORMAT = "yyyy/MM/dd HH:mm:ss";

  @Test
  void testFilter() throws Exception {
    final TxFilterResponse txFilterResponse = new TxFilterResponse();

    txFilterResponse.setHash("a43355eef79cce2223b4f234fbc4f98bad770e0bbedb3f5a00148fec7e815529");
    txFilterResponse.setBlockNo(9086101L);
    txFilterResponse.setBlockHash(
        "fcbf413ed6236bb32c2386c220e6d6c4882309f8e83386e2ec40c57a0a8e8f21");
    txFilterResponse.setEpochNo(426);
    txFilterResponse.setEpochSlotNo(292728);
    txFilterResponse.setSlot(98961528);
    txFilterResponse.setFee(BigInteger.valueOf(315142));
    txFilterResponse.setTokens(new ArrayList<>());
    txFilterResponse.setTime(LocalDateTime.parse("2007-12-03T10:15:30"));
    final BaseFilterResponse<TxFilterResponse> response =
        new BaseFilterResponse<>(new PageImpl<>(List.of(txFilterResponse)));

    when(mockTxService.getAll(any(Pageable.class))).thenReturn(response);

    var result =
        mockMvc
            .perform(get("/api/v1/txs").accept(MediaType.APPLICATION_JSON))
            .andReturn()
            .getResponse();

    assertEquals(HttpStatus.OK.value(), result.getStatus());
  }

  @Test
  void testGetTransactionDetail() throws Exception {
    final String hash = "847e7236454aea46714b07c4b299e2567940cd23c508db777f17a0d9a85d60a6";
    final TxResponse txResponse = new TxResponse();

    txResponse.setTx(
        TxInfoResponse.builder()
            .hash("847e7236454aea46714b07c4b299e2567940cd23c508db777f17a0d9a85d60a6")
            .build());
    txResponse.setSummary(SummaryResponse.builder().build());
    txResponse.setUTxOs(UTxOResponse.builder().build());
    txResponse.setContracts(null);
    txResponse.setCollaterals(CollateralResponse.builder().build());

    when(mockTxService.getTxDetailByHash(hash)).thenReturn(txResponse);

    final MockHttpServletResponse response =
        mockMvc
            .perform(get("/api/v1/txs/{hash}", hash).accept(MediaType.APPLICATION_JSON))
            .andReturn()
            .getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());
  }

  @Test
  void testFindCurrentTransaction() throws Exception {
    TxSummary txSummary =
        TxSummary.builder()
            .hash("5d43c58bd68758cae8efe75a2c3099736260d29bc46b90371069245c8e9d3f7f")
            .blockNo(9086598L)
            .slot(98972043)
            .amount(209644042.0)
            .time(LocalDateTime.parse("2023-07-28T10:15:30"))
            .fromAddress(
                List.of(
                    "addr1qxg4eahp6lt8hdj0asv0tyxkkh3zsj9jxkykx5fc8pscarc22wrtjv4lxs8slf94fcvv7ecpguatkza5h2p3qvuug8qql9hxzr"))
            .toAddress(
                List.of(
                    "addr1qxg4eahp6lt8hdj0asv0tyxkkh3zsj9jxkykx5fc8pscarc22wrtjv4lxs8slf94fcvv7ecpguatkza5h2p3qvuug8qql9hxzr"))
            .build();
    when(mockTxService.findLatestTxSummary()).thenReturn(List.of(txSummary));

    final MockHttpServletResponse response =
        mockMvc
            .perform(get("/api/v1/txs/current").accept(MediaType.APPLICATION_JSON))
            .andReturn()
            .getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());
  }

  @Test
  void testFindCurrentTransaction_WhenTxServiceReturnsNoItems() throws Exception {
    when(mockTxService.findLatestTxSummary()).thenReturn(Collections.emptyList());

    final MockHttpServletResponse response =
        mockMvc
            .perform(get("/api/v1/txs/current").accept(MediaType.APPLICATION_JSON))
            .andReturn()
            .getResponse();

    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals("[]", response.getContentAsString());
  }

  private static String asJsonString(final Object obj) {
    try {
      var objectMapper = new ObjectMapper();
      objectMapper.setDateFormat(new SimpleDateFormat(DATE_TIME_FORMAT));
      objectMapper.setSerializationInclusion(Include.NON_NULL);
      objectMapper.registerModule(new JavaTimeModule());
      return objectMapper.writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
