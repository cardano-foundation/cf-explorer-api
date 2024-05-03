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
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.*;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyLifeCycleServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.RewardType;

@WebMvcTest(StakeKeyLifeCycleController.class)
@AutoConfigureMockMvc(addFilters = false)
@Import(RoleFilterMapper.class)
class StakeKeyLifeCycleControllerTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private StakeKeyLifeCycleServiceImpl stakeKeyLifeCycleService;

  @MockBean private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetStakeLifeCycle() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    StakeLifecycleResponse response =
        StakeLifecycleResponse.builder()
            .hasRegistration(true)
            .hashRewards(true)
            .totalDelegatorRewards(BigInteger.TWO)
            .build();

    when(stakeKeyLifeCycleService.getStakeLifeCycle(stakeKey)).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/stake-lifecycle/{stakeKey}", stakeKey))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.hasRegistration").value(true))
        .andExpect(jsonPath("$.hashRewards").value(true))
        .andExpect(jsonPath("$.totalDelegatorRewards").value(BigInteger.TWO));

    verify(stakeKeyLifeCycleService).getStakeLifeCycle(stakeKey);
  }

  @Test
  void shouldGetRegistrations() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeRegistrationFilterResponse> list = new ArrayList<>();
    list.add(
        StakeRegistrationFilterResponse.builder()
            .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
            .deposit(2000000L)
            .fee(BigInteger.valueOf(173333))
            .time(LocalDateTime.now())
            .build());
    given(
            stakeKeyLifeCycleService.getStakeRegistrations(
                stakeKey, filter, PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/registrations", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetRegistrationDetail() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    StakeRegistrationDetailResponse registration =
        StakeRegistrationDetailResponse.builder()
            .txHash(txHash)
            .deposit(2000000L)
            .fee(BigInteger.valueOf(173333))
            .time(LocalDateTime.now())
            .joinDepositPaid(true)
            .build();
    given(stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, txHash))
        .willReturn(registration);
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/registrations/{hash}", stakeKey, txHash)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetDeRegistrations() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    List<StakeRegistrationFilterResponse> list = new ArrayList<>();
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    list.add(
        StakeRegistrationFilterResponse.builder()
            .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
            .deposit(-2000000L)
            .fee(BigInteger.valueOf(173333))
            .time(LocalDateTime.now())
            .build());
    given(
            stakeKeyLifeCycleService.getStakeDeRegistrations(
                stakeKey, filter, PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/de-registrations", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(result -> System.out.println(result.getResponse().getContentAsString()));
  }

  @Test
  void shouldGetDeRegistrationDetail() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    StakeRegistrationDetailResponse registration =
        StakeRegistrationDetailResponse.builder()
            .txHash(txHash)
            .deposit(2000000L)
            .fee(BigInteger.valueOf(173333))
            .time(LocalDateTime.now())
            .joinDepositPaid(true)
            .build();
    given(stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, txHash))
        .willReturn(registration);
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/de-registrations/{hash}", stakeKey, txHash)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetDelegations() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeDelegationFilterResponse> list = new ArrayList<>();
    list.add(
        StakeDelegationFilterResponse.builder()
            .txHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")
            .outSum(BigInteger.valueOf(102569063))
            .time(LocalDateTime.now())
            .build());
    given(
            stakeKeyLifeCycleService.getStakeDelegations(
                stakeKey, filter, PageRequest.of(0, 1, Sort.by("tx").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/delegations", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")))
        .andDo(print());
  }

  //  @Test
  //  void shouldGetDelegationDetail() throws Exception {
  //    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
  //    String hash = "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488";
  //    StakeDelegationDetailResponse response =
  //        StakeDelegationDetailResponse.builder()
  //            .txHash(hash)
  //            .outSum(BigInteger.valueOf(102569063))
  //            .time(LocalDateTime.now())
  //            .stakeTotalAmount(BigInteger.valueOf(102569063))
  //            .poolId("pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu")
  //            .poolName("The HIGH Pool")
  //            .epoch(369)
  //            .blockNo(7895711L)
  //            .fee(BigInteger.valueOf(173333))
  //            .build();
  //    given(
  //            stakeKeyLifeCycleService.getStakeDelegationDetail(
  //                stakeKey, "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488"))
  //        .willReturn(response);
  //    mockMvc
  //        .perform(
  //            get("/api/v1/stake-lifecycle/{stakeKey}/delegations/{hash}", stakeKey, hash)
  //                .contentType(MediaType.APPLICATION_JSON))
  //        .andExpect(status().isOk())
  //        .andExpect(
  //            content()
  //                .string(
  //                    containsString(
  //                        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")))
  //        .andDo(print());
  //  }

  @Test
  void shouldGetRewards() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date fromDate = sdf.parse("2023/01/01 00:00:00");
    Date toDate = sdf.parse("2023/01/01 00:00:00");
    List<StakeRewardResponse> list = new ArrayList<>();
    list.add(new StakeRewardResponse(333, toDate, BigInteger.valueOf(382916), RewardType.MEMBER));
    given(
            stakeKeyLifeCycleService.getStakeRewards(
                stakeKey, fromDate, toDate, null, PageRequest.of(0, 1, Sort.by("id").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/rewards", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .param("fromDate", "2023/01/01 00:00:00")
                .param("toDate", "2023/01/01 00:00:00")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString("333")))
        .andDo(print());
  }

  @Test
  void shouldGetWithdrawals() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeWithdrawalFilterResponse> list = new ArrayList<>();
    list.add(
        StakeWithdrawalFilterResponse.builder()
            .txHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")
            .value(BigInteger.valueOf(4846486))
            .time(LocalDateTime.now())
            .build());
    given(
            stakeKeyLifeCycleService.getStakeWithdrawals(
                stakeKey, filter, PageRequest.of(0, 1, Sort.by("id").descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/withdrawals", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")))
        .andDo(print());
  }

  //  @Test
  //  void shouldGetWithdrawalDetail() throws Exception {
  //    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
  //    String hash = "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381";
  //    StakeWithdrawalDetailResponse response =
  //        StakeWithdrawalDetailResponse.builder()
  //            .txHash(hash)
  //            .amount(BigInteger.valueOf(4846486))
  //            .stakeTotalAmount(BigInteger.valueOf(102569063))
  //            .stakeRewardAvailable(BigInteger.valueOf(4846486))
  //            .fee(BigInteger.valueOf(173333))
  //            .time(LocalDateTime.now())
  //            .build();
  //    given(
  //            stakeKeyLifeCycleService.getStakeWithdrawalDetail(
  //                stakeKey, "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381"))
  //        .willReturn(response);
  //    mockMvc
  //        .perform(
  //            get("/api/v1/stake-lifecycle/{stakeKey}/withdrawals/{hash}", stakeKey, hash)
  //                .contentType(MediaType.APPLICATION_JSON))
  //        .andExpect(status().isOk())
  //        .andExpect(content().string(containsString(hash)))
  //        .andDo(print());
  //  }

  //  @Test
  //  void testGetWalletActivities() throws Exception {
  //    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
  //    StakeWalletActivityResponse response = new StakeWalletActivityResponse();
  //    response.setTxHash("8e85eb8f7de457868ab64c2d8c07257252063d517c4d4e1a01aa2af5783e9bda");
  //    response.setAmount(BigInteger.ONE);
  //    response.setStatus(TxStatus.SUCCESS);
  //
  //    when(stakeKeyLifeCycleService.getStakeWalletActivities(
  //            stakeKey, PageRequest.of(0, 1, Sort.by("tx").descending())))
  //        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));
  //
  //    mockMvc
  //        .perform(
  //            get("/api/v1/stake-lifecycle/{stakeKey}/wallet-activity", stakeKey)
  //                .param("page", "0")
  //                .param("size", "1")
  //                .param("sort", "tx,DESC")
  //                .contentType(MediaType.APPLICATION_JSON))
  //        .andExpect(status().isOk())
  //        .andExpect(
  //            jsonPath("$.data[0].txHash")
  //                .value("8e85eb8f7de457868ab64c2d8c07257252063d517c4d4e1a01aa2af5783e9bda"))
  //        .andExpect(jsonPath("$.data[0].status").value(TxStatus.SUCCESS.toString()))
  //        .andExpect(jsonPath("$.data[0].amount").value(1));
  //
  //    verify(stakeKeyLifeCycleService)
  //        .getStakeWalletActivities(stakeKey, PageRequest.of(0, 1, Sort.by("tx").descending()));
  //  }

  @Test
  void testGetRewardActivities() throws Exception {
    String stakeKey = "stake_test1upa9qlj5ljhx7w6f0h0k083f69cd442fqhseh08m05ucw4sx9t94t";
    StakeRewardActivityResponse response =
        StakeRewardActivityResponse.builder()
            .type(StakeRewardType.REWARD_RECEIVED)
            .epochNo(1)
            .amount(BigInteger.ONE)
            .build();

    when(stakeKeyLifeCycleService.getStakeRewardActivities(
            stakeKey, PageRequest.of(0, 1, Sort.by("time").descending())))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/stake-lifecycle/{stakeKey}/reward-activity", stakeKey)
                .param("page", "0")
                .param("size", "1")
                .param("sort", "time,DESC")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].epochNo").value(1))
        .andExpect(jsonPath("$.data[0].amount").value(BigInteger.ONE))
        .andExpect(jsonPath("$.data[0].type").value(StakeRewardType.REWARD_RECEIVED.toString()));

    verify(stakeKeyLifeCycleService)
        .getStakeRewardActivities(stakeKey, PageRequest.of(0, 1, Sort.by("time").descending()));
  }
}
