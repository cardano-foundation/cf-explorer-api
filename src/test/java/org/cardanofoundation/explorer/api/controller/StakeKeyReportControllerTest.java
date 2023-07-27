package org.cardanofoundation.explorer.api.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.*;

import org.cardanofoundation.explorer.api.common.enumeration.ExportType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.model.request.stake.report.StakeKeyReportRequest;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.ReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.stake.report.StakeKeyReportResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeDelegationFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRegistrationLifeCycle;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWalletActivityResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeWithdrawalFilterResponse;
import org.cardanofoundation.explorer.api.service.ReportHistoryService;
import org.cardanofoundation.explorer.api.service.StakeKeyReportService;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
import org.junit.jupiter.api.Assertions;
import org.mockito.BDDMockito;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(StakeKeyReportController.class)
@AutoConfigureMockMvc(addFilters = false)
class StakeKeyReportControllerTest {

  private final String END_POINT = "/api/v1/staking-lifecycle/report";

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private StakeKeyReportService stakeKeyReportService;

  @MockBean
  private ReportHistoryService reportHistoryService;

  @MockBean
  private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void shouldGenerateStakeKeyReport() throws Exception {
    String username = "username";
    Timestamp fromDateTimestamp = Timestamp.valueOf("2021-01-01 00:00:00");
    Timestamp toDateTimestamp = Timestamp.valueOf("2021-02-01 00:00:00");

    StakeKeyReportHistoryResponse responseExpect = StakeKeyReportHistoryResponse.builder()
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .fromDate(fromDateTimestamp)
        .toDate(toDateTimestamp)
        .isADATransfer(Boolean.TRUE)
        .eventRegistration(Boolean.TRUE)
        .status(ReportStatus.IN_PROGRESS)
        .type(ReportType.STAKE_KEY)
        .build();

    BDDMockito.given(
            stakeKeyReportService.generateStakeKeyReport(any(StakeKeyReportRequest.class), anyString()))
        .willReturn(responseExpect);

    mockMvc.perform(post(END_POINT + "/stake-key")
            .contentType(MediaType.APPLICATION_JSON)
            .content(asJsonString(Map.of("fromDate", "2021/01/01 00:00:00", "toDate", "2021/02/01 00:00:00")))
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")))
        .andDo(print());
  }

  @Test
  void shouldExportStakeKeyReportByStorageKey() throws Exception {
    String username = "username";
    Long reportId = 1L;
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream("test".getBytes());
    StakeKeyReportResponse responseExpect = StakeKeyReportResponse.builder()
        .fileName("stake-key-report.csv")
        .byteArrayInputStream(byteArrayInputStream)
        .build();

    given(stakeKeyReportService.exportStakeKeyReport(anyLong(), anyString(), any(ExportType.class)))
        .willReturn(responseExpect);

    MvcResult result = mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/export", reportId)
            .contentType(MediaType.APPLICATION_JSON)
            .param("exportType", ExportType.CSV.name())
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andReturn();
    Assertions.assertEquals(4, result.getResponse().getContentLength());
    Assertions.assertTrue(
        result.getResponse().getHeader("Content-Disposition").split("=")[1].contains(
            responseExpect.getFileName()));
    Assertions.assertEquals(MediaType.APPLICATION_OCTET_STREAM_VALUE,
        result.getResponse().getContentType());
  }

  @Test
  void shouldGetStakeKeyReportDetail() throws Exception {
    Long reportId = 1L;
    String username = "username";
    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = getStakeKeyReportHistoryResponse(
        reportId, username);

    BDDMockito.given(stakeKeyReportService.getStakeKeyReportHistoryByReportId(reportId, username))
        .willReturn(stakeKeyReportHistoryResponse);

    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/detail", reportId)
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")))
        .andDo(print());
  }


  @Test
  void shouldGetStakeKeyReportHistoriesByStakeKey() throws Exception {
    Long reportId = 1L;
    String username = "username";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = getStakeKeyReportHistoryResponse(
        reportId, username);

    List<StakeKeyReportHistoryResponse> responseList = Collections.singletonList(
        stakeKeyReportHistoryResponse);

    BDDMockito.given(stakeKeyReportService.getStakeKeyReportHistoryByStakeKey(stakeKey, username,
            PageRequest.of(0, 1)))
        .willReturn(new BaseFilterResponse<>(responseList, 1, 1, 0));

    mockMvc.perform(get(END_POINT + "/stake-key/{stakeKey}/history", stakeKey)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")))
        .andDo(print());
  }


  @Test
  void shouldGetAllStakeKeyReportHistories() throws Exception {
    Long reportId = 1L;
    String username = "username";
    SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");

    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = getStakeKeyReportHistoryResponse(
        reportId, username);

    List<StakeKeyReportHistoryResponse> responseList = Collections.singletonList(
        stakeKeyReportHistoryResponse);

    given(stakeKeyReportService.getStakeKeyReportHistory(any(), any(), any()))
        .willReturn(new BaseFilterResponse<>(responseList, 1, 1, 0));

    mockMvc.perform(get(END_POINT + "/stake-key/history")
            .param("page", "0")
            .param("size", "1")
            .param("fromDate", "2020/01/01")
            .param("toDate", "2021/01/01")
            .param("reportName", "")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")))
        .andDo(print());
  }

  @Test
  void shouldGetReportHistoryDashBoard() throws Exception {
    String username = "username";

    ReportHistoryResponse response = ReportHistoryResponse.builder()
        .reportName("report_name_123")
        .stakeKeyReportId(1L)
        .type(ReportType.STAKE_KEY)
        .build();

    List<ReportHistoryResponse> responseList = Collections.singletonList(response);

    BDDMockito.given(
            reportHistoryService.getReportHistory(any(ReportHistoryFilterRequest.class), anyString(),
                any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(responseList, 1, 1, 0));

    mockMvc.perform(get(END_POINT + "/dashboard")
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("report_name_123")))
        .andDo(print());
  }

  @Test
  void shouldGetRegistrations() throws Exception {
    Long reportId = 1L;
    String username = "username";
    List<StakeRegistrationLifeCycle> list = new ArrayList<>();
    list.add(StakeRegistrationLifeCycle.builder()
        .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
        .deposit(2000000L)
        .fee(BigInteger.valueOf(173333))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyReportService.getStakeRegistrationsByReportId(reportId, username,
        PageRequest.of(0, 1, Sort.by("time")
            .descending())))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/registrations", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetDeRegistrations() throws Exception {
    String username = "username";
    Long reportId = 1L;
    List<StakeRegistrationLifeCycle> list = new ArrayList<>();
    list.add(StakeRegistrationLifeCycle.builder()
        .txHash("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")
        .deposit(-2000000L)
        .fee(BigInteger.valueOf(173333))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyReportService.getStakeDeRegistrationsByReportId(anyLong(), anyString(),
        any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/de-registrations", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2")))
        .andDo(print());
  }

  @Test
  void shouldGetDelegations() throws Exception {
    Long reportId = 1L;
    String username = "username";
    List<StakeDelegationFilterResponse> list = new ArrayList<>();
    list.add(StakeDelegationFilterResponse.builder()
        .txHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")
        .outSum(BigInteger.valueOf(102569063))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyReportService.getStakeDelegationsByReportId(anyLong(), anyString(),
        any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/delegations", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))

        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488")))
        .andDo(print());
  }

  @Test
  void shouldGetRewards() throws Exception {
    Long reportId = 1L;
    String username = "username";
    List<StakeRewardResponse> list = new ArrayList<>();
    list.add(new StakeRewardResponse(333, Date.from(Instant.now()), BigInteger.valueOf(382916), RewardType.MEMBER));
    given(stakeKeyReportService.getStakeRewardsByReportId(anyLong(), anyString(),
        any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));

    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/rewards", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("333")))
        .andDo(print());
  }

  @Test
  void shouldGetWithdrawals() throws Exception {
    Long reportId = 1L;
    String username = "username";
    StakeLifeCycleFilterRequest filter = new StakeLifeCycleFilterRequest();
    List<StakeWithdrawalFilterResponse> list = new ArrayList<>();
    list.add(StakeWithdrawalFilterResponse.builder()
        .txHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")
        .value(BigInteger.valueOf(4846486))
        .time(LocalDateTime.now())
        .build());
    given(stakeKeyReportService.getStakeWithdrawalsByReportId(anyLong(), anyString(),
        any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/withdrawals", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")))
        .andDo(print());
  }

  @Test
  void shouldGetWalletActivity() throws Exception {
    Long reportId = 1L;
    String username = "username";
    List<StakeWalletActivityResponse> list = new ArrayList<>();
    StakeWalletActivityResponse response = new StakeWalletActivityResponse();
    response.setTxHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    response.setStatus(TxStatus.SUCCESS);
    response.setType(StakeTxType.SENT);
    response.setTime(LocalDateTime.now());
    response.setFee(BigInteger.valueOf(1000000));
    response.setAmount(BigInteger.valueOf(1000000));
    list.add(response);
    given(stakeKeyReportService.getWalletActivitiesByReportId(anyLong(), anyString(),
        any(Pageable.class)))
        .willReturn(new BaseFilterResponse<>(list, 1, 1, 0));
    mockMvc.perform(get(END_POINT + "/stake-key/{reportId}/wallet-activity", reportId)
            .param("page", "0")
            .param("size", "1")
            .contentType(MediaType.APPLICATION_JSON)
            .with(request -> {
              request.setAttribute("username", username);
              return request;
            }))
        .andExpect(status().isOk())
        .andExpect(content().string(
            containsString("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381")))
        .andDo(print());
  }

  private StakeKeyReportHistoryResponse getStakeKeyReportHistoryResponse(Long reportId,
      String username) {
    StakeKeyReportHistoryResponse stakeKeyReportHistoryResponse = StakeKeyReportHistoryResponse.builder()
        .id(reportId)
        .stakeKey("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
        .username(username)
        .reportName("test")
        .fromDate(new Timestamp(System.currentTimeMillis()))
        .toDate(new Timestamp(System.currentTimeMillis()))
        .isADATransfer(Boolean.TRUE)
        .isFeesPaid(Boolean.FALSE)
        .eventRegistration(Boolean.TRUE)
        .eventDelegation(Boolean.TRUE)
        .eventRewards(Boolean.TRUE)
        .eventWithdrawal(Boolean.TRUE)
        .eventDeregistration(Boolean.FALSE)
        .build();
    return stakeKeyReportHistoryResponse;
  }

  public static String asJsonString(final Object obj) {
    try {
      return new ObjectMapper().writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}