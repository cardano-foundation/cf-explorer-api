package com.cardano.explorer.controller;


import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalDetailResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import com.cardano.explorer.service.impl.StakeKeyLifeCycleServiceImpl;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@WebMvcTest(StakeKeyLifeCycleController.class)
class StakeKeyLifeCycleControllerTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private StakeKeyLifeCycleServiceImpl stakeKeyLifeCycleService;

  @Test
  void shouldGetRegistrations() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeRegistrationLifeCycle> list = new ArrayList<>();
    list.add(StakeRegistrationLifeCycle.builder()
        .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
        .deposit(2000000L)
        .fee(BigInteger.valueOf(173333))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyLifeCycleService.getStakeRegistrations(stakeKey, filter,
        PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/registrations", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetDeRegistrations() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    List<StakeRegistrationLifeCycle> list = new ArrayList<>();
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    list.add(StakeRegistrationLifeCycle.builder()
        .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
        .deposit(-2000000L)
        .fee(BigInteger.valueOf(173333))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyLifeCycleService.getStakeDeRegistrations(stakeKey, filter,
        PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/de-registrations", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(result -> System.out.println(result.getResponse().getContentAsString()));
  }

  @Test
  void shouldGetDelegations() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeDelegationFilterResponse> list = new ArrayList<>();
    list.add(StakeDelegationFilterResponse.builder()
        .txHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")
        .outSum(BigInteger.valueOf(102569063))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyLifeCycleService.getStakeDelegations(stakeKey, filter,
        PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/delegations", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")))
        .andDo(print());
  }

  @Test
  void shouldGetDelegationDetail() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    String hash = "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488";
    StakeDelegationDetailResponse response = StakeDelegationDetailResponse.builder()
        .txHash(hash)
        .outSum(BigInteger.valueOf(102569063))
        .time(LocalDateTime.now())
        .stakeTotalAmount(BigInteger.valueOf(102569063))
        .poolId("pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu")
        .poolName("The HIGH Pool")
        .epoch(369)
        .blockNo(7895711L)
        .fee(BigInteger.valueOf(173333))
        .build();
    given(stakeKeyLifeCycleService.getStakeDelegationDetail(stakeKey,
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")).willReturn(response);
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/delegations/{hash}", stakeKey, hash)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")))
        .andDo(print());
  }

  @Test
  void shouldGetRewards() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    List<StakeRewardResponse> list = new ArrayList<>();
    list.add(new StakeRewardResponse(333,
        Date.from(Instant.now()), BigInteger.valueOf(382916)));
    given(stakeKeyLifeCycleService.getStakeRewards(stakeKey,
        PageRequest.of(0, 1, Sort.by("id").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/rewards", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("333")))
        .andDo(print());
  }

  @Test
  void shouldGetWithdrawals() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeWithdrawalFilterResponse> list = new ArrayList<>();
    list.add(StakeWithdrawalFilterResponse.builder()
        .txHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")
        .value(BigInteger.valueOf(4846486))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyLifeCycleService.getStakeWithdrawals(stakeKey, filter,
        PageRequest.of(0, 1, Sort.by("id").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/withdrawals", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")))
        .andDo(print());
  }

  @Test
  void shouldGetWithdrawalDetail() throws Exception {
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    String hash = "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381";
    StakeWithdrawalDetailResponse response = StakeWithdrawalDetailResponse.builder()
        .txHash(hash)
        .amount(BigInteger.valueOf(4846486))
        .stakeTotalAmount(BigInteger.valueOf(102569063))
        .stakeRewardAvailable(BigInteger.valueOf(4846486))
        .fee(BigInteger.valueOf(173333))
        .time(LocalDateTime.now())
        .build();
    given(stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey,
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")).willReturn(response);
    mockMvc.perform(get("/api/v1/stake-lifecycle/{stakeKey}/withdrawals/{hash}", stakeKey, hash)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString(hash)))
        .andDo(print());
  }

}
