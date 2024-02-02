package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import org.mockito.Mockito;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolTxResponse;
import org.cardanofoundation.explorer.api.model.response.pool.TxPoolCertificateHistory;
import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import org.cardanofoundation.explorer.api.service.PoolCertificateService;
import org.cardanofoundation.explorer.api.service.PoolRegistrationService;

@WebMvcTest(PoolController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class PoolControllerTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private PoolRegistrationService poolRegistrationService;

  @MockBean private PoolCertificateService poolCertificateService;

  @MockBean private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void whenCallRegistration() throws Exception {
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);
    when(projection.getTxHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTxTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getEpochNo()).thenReturn(420);
    when(projection.getSlotNo()).thenReturn(120000);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    BaseFilterResponse<PoolTxResponse> res = new BaseFilterResponse<>();
    List<PoolTxResponse> dataList = new ArrayList<>();
    dataList.add(new PoolTxResponse(projection));
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolRegistrationService.getDataForPoolRegistration(PageRequest.of(0, 1))).willReturn(res);
    mockMvc
        .perform(get("/api/v1/pools/registration").param("page", "0").param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void whenCallDeRegistration() throws Exception {
    TxBlockEpochProjection projection = Mockito.mock(TxBlockEpochProjection.class);
    when(projection.getTxId()).thenReturn(1L);
    when(projection.getTxHash())
        .thenReturn("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTxTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getEpochNo()).thenReturn(420);
    when(projection.getSlotNo()).thenReturn(120000);
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.TWO);
    when(projection.getPoolView())
        .thenReturn("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    BaseFilterResponse<PoolTxResponse> res = new BaseFilterResponse<>();
    List<PoolTxResponse> dataList = new ArrayList<>();
    dataList.add(new PoolTxResponse(projection));
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolRegistrationService.getDataForPoolDeRegistration(PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc
        .perform(get("/api/v1/pools/de-registration").param("page", "0").param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void testGetTxPoolCertificatesHistory() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    TxPoolCertificateHistory txPoolCertificateHistory = new TxPoolCertificateHistory();
    txPoolCertificateHistory.setTxHash(
        "a151dbcfa201c1980d112809531e2a82e941895839460ba2efdfe905015eacdc");
    txPoolCertificateHistory.setBlockNo(5699952L);
    txPoolCertificateHistory.setEpochSlotNo(403614);
    txPoolCertificateHistory.setSlotNo(29088414);
    txPoolCertificateHistory.setTxEpochNo(264);
    PageRequest pageable = PageRequest.of(0, 1, Sort.by("createdAt").descending());

    when(poolCertificateService.getTxPoolCertificateHistory(poolView, pageable))
        .thenReturn(new BaseFilterResponse<>(List.of(txPoolCertificateHistory), 1));

    mockMvc
        .perform(
            get("/api/v1/pools/certificates-history/{poolViewOrHash}", poolView)
                .param("page", "0")
                .param("size", "1")
                .param("sort", "createdAt,DESC")
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "a151dbcfa201c1980d112809531e2a82e941895839460ba2efdfe905015eacdc")))
        .andExpect(jsonPath("$.data[0].blockNo").value(5699952L));

    verify(poolCertificateService).getTxPoolCertificateHistory(poolView, pageable);
  }
}
