package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.security.configuration.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.DeRegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolInfoResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateDetailResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.PoolUpdateResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RegistrationResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.RewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.SPOStatusResponse;
import org.cardanofoundation.explorer.api.model.response.pool.lifecycle.TabularRegisResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.LifeCycleRewardProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;
import org.cardanofoundation.explorer.api.service.PoolLifecycleService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.web.servlet.MockMvc;

@WebMvcTest(PoolLifecycleController.class)
@Import({
    SpringWebSecurityConfig.class,
    WebConfig.class,
    JacksonMapperDateConfig.class,
    GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
class PoolLifecycleControllerTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private PoolLifecycleService poolLifecycleService;

  @MockBean
  private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void whenCallRegistrations() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    BaseFilterResponse<PoolUpdateResponse> res = new BaseFilterResponse<>();
    List<PoolUpdateResponse> dataList = new ArrayList<>();
    dataList.add(new PoolUpdateResponse(projection));
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.registration(poolView, null, null, null,
        PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/registration")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void whenCallRegistrationDetail() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolRegistrationProjection projection = Mockito.mock(PoolRegistrationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getFee()).thenReturn(BigInteger.ZERO);
    when(projection.getDeposit()).thenReturn(BigInteger.TWO);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.ONE);
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    RegistrationResponse res = new RegistrationResponse(projection);
    res.setPoolId("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    res.setPoolName("Test");
    res.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    given(poolLifecycleService.registrationDetail(poolView, 1L))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/registration-detail")
            .param("poolView", poolView)
            .param("id", "1"))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e")));
  }

  @Test
  void whenCallPoolUpdate() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolUpdateProjection projection = Mockito.mock(PoolUpdateProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getFee()).thenReturn(BigInteger.TEN);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getMargin()).thenReturn(1.0);
    BaseFilterResponse<PoolUpdateResponse> res = new BaseFilterResponse<>();
    List<PoolUpdateResponse> dataList = new ArrayList<>();
    dataList.add(new PoolUpdateResponse(projection));
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.poolUpdate(poolView, null, null, null,
        PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/pool-update")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void whenCallPoolUpdateDetail() throws Exception {
    Long id = 1L;
    PoolUpdateDetailProjection projection = Mockito.mock(PoolUpdateDetailProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getFee()).thenReturn(BigInteger.ZERO);
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.ONE);
    PoolUpdateDetailResponse res = new PoolUpdateDetailResponse(projection);
    res.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    res.setPreviousMargin(0.15);
    res.setPreviousPledge(BigInteger.ONE);
    given(poolLifecycleService.poolUpdateDetail(id))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/pool-update-detail")
            .param("id", "1"))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e")));
  }


  @Test
  void whenCallReward() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    LifeCycleRewardProjection projection = Mockito.mock(LifeCycleRewardProjection.class);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getEpochNo()).thenReturn(1);
    when(projection.getAmount()).thenReturn(BigInteger.TEN);
    when(projection.getAddress()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    BaseFilterResponse<RewardResponse> res = new BaseFilterResponse<>();
    List<RewardResponse> dataList = new ArrayList<>();
    dataList.add(new RewardResponse(projection));
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.listReward(poolView, PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/reward")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p")));
  }

  @Test
  void whenCallDeRegistrations() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolDeRegistrationProjection projection = Mockito.mock(PoolDeRegistrationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getFee()).thenReturn(BigInteger.ZERO);
    when(projection.getRetiringEpoch()).thenReturn(1);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    DeRegistrationResponse deRegistrationRes = new DeRegistrationResponse(projection);
    BaseFilterResponse<DeRegistrationResponse> res = new BaseFilterResponse<>();
    deRegistrationRes.setPoolId("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    deRegistrationRes.setPoolName("Test");
    deRegistrationRes.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    deRegistrationRes.setPoolHold(BigInteger.TEN);
    deRegistrationRes.setPoolView("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    List<DeRegistrationResponse> dataList = new ArrayList<>();
    dataList.add(deRegistrationRes);
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.deRegistration(poolView, null, null, null,
        PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/de-registration")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
//        .andExpect(content().string(
//            containsString("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e")));
  }

  @Test
  void whenCallPoolInfo() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolInfoResponse res = new PoolInfoResponse();
    res.setPoolId("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    res.setPoolName("Test");
    res.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    res.setPoolSize(BigInteger.TEN);
    res.setPoolView("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    res.setRewardAvailable(BigInteger.ONE);
    res.setEpochNo(1);
    res.setStatus("ACTIVE");
    given(poolLifecycleService.poolInfo(poolView))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/pool-info")
            .param("poolView", poolView))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535")));
  }

  @Test
  void whenCallRegistrationList() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolRegistrationProjection projection = Mockito.mock(PoolRegistrationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getFee()).thenReturn(BigInteger.ZERO);
    when(projection.getDeposit()).thenReturn(BigInteger.TWO);
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getPoolUpdateId()).thenReturn(1L);
    TabularRegisResponse tabularRegisRes = new TabularRegisResponse(projection);
    tabularRegisRes.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    BaseFilterResponse<TabularRegisResponse> res = new BaseFilterResponse<>();
    List<TabularRegisResponse> dataList = new ArrayList<>();
    dataList.add(tabularRegisRes);
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.registrationList(poolView, PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/registration-list")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void whenCallPoolUpdateList() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    PoolUpdateDetailProjection projection = Mockito.mock(PoolUpdateDetailProjection.class);
    when(projection.getPoolUpdateId()).thenReturn(1L);
    when(projection.getPoolId()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b803asdasdw34535");
    when(projection.getPoolName()).thenReturn("Test");
    when(projection.getPoolView()).thenReturn(
        "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    when(projection.getTxHash()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60b7e");
    when(projection.getTime()).thenReturn(Timestamp.from(Instant.now()));
    when(projection.getFee()).thenReturn(BigInteger.ZERO);
    when(projection.getRewardAccount()).thenReturn(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p");
    when(projection.getVrfKey()).thenReturn(
        "d867f77bb62fe58df4b13285f6b8d37a8aae41eea662b248b80321ec5ce60asda");
    when(projection.getPledge()).thenReturn(BigInteger.TEN);
    when(projection.getMargin()).thenReturn(0.1);
    when(projection.getCost()).thenReturn(BigInteger.ONE);
    PoolUpdateDetailResponse poolUpdateDetailRes = new PoolUpdateDetailResponse(projection);
    poolUpdateDetailRes.setStakeKeys(Collections.singletonList(
        "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p"));
    BaseFilterResponse<PoolUpdateDetailResponse> res = new BaseFilterResponse<>();
    List<PoolUpdateDetailResponse> dataList = new ArrayList<>();
    dataList.add(poolUpdateDetailRes);
    res.setTotalItems(1);
    res.setData(dataList);
    given(poolLifecycleService.poolUpdateList(poolView, PageRequest.of(0, 1)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/pool-update-list")
            .param("poolView", poolView)
            .param("page", "0")
            .param("size", "1"))
        .andDo(print())
        .andExpect(status().isOk());
  }

  @Test
  void whenCallPoolOwner() throws Exception {
    String stakeKey = "stake1u80n7nvm3qlss9ls0krp5xh7sqxlazp8kz6n3fp5sgnul5cnxyg4p";
    BaseFilterResponse<String> res = new BaseFilterResponse<>();
    List<String> dataList = new ArrayList<>();
    dataList.add("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s");
    dataList.add("pool1wz27a9pvd2jat98s375lxaaj7qxy405km4vzp83wn5vtwp59kvh");
    dataList.add("pool1qeyjycp9ef0drrzaq3u9ylwclqa56zl5n4yd75txw92csug3mzn");
    res.setTotalItems(3);
    res.setData(dataList);
    given(poolLifecycleService.getPoolViewByStakeKey(stakeKey, PageRequest.of(0, 10)))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/owner")
            .param("stakeKey", stakeKey)
            .param("page", "0")
            .param("size", "10"))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s")));
  }

  @Test
  void whenCallPoolStatus() throws Exception {
    String poolView = "pool1h0anq89dytn6vtm0afhreyawcnn0w99w7e4s4q5w0yh3ymzh94s";
    SPOStatusResponse res = new SPOStatusResponse();
    res.setIsRegistration(true);
    res.setIsUpdate(true);
    res.setIsReward(true);
    res.setIsDeRegistration(false);
    given(poolLifecycleService.poolLifecycleStatus(poolView))
        .willReturn(res);
    mockMvc.perform(get("/api/v1/pool-lifecycle/status")
            .param("poolView", poolView))
        .andDo(print())
        .andExpect(status().isOk())
        .andExpect(content().json(
            "{\"isRegistration\":true,\"isUpdate\":true,\"isReward\":true,\"isDeRegistration\":false}"));
  }
}
