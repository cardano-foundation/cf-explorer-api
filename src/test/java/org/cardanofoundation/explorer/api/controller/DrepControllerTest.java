package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;
import java.sql.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDelegatorsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.drep.VotingProcedureChartResponse;
import org.cardanofoundation.explorer.api.service.DRepService;

@WebMvcTest(DRepController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class DrepControllerTest {
  @MockBean private AuthInterceptor authInterceptor;
  @Autowired private MockMvc mockMvc;
  @MockBean private DRepService dRepService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetTxDRepCertificatesHistory() throws Exception {
    String drepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    DRepCertificateHistoryResponse dRepCertificateHistoryResponse1 =
        DRepCertificateHistoryResponse.builder()
            .txHash(drepHash)
            .blockNo(100L)
            .createdAt(new Date(10001))
            .absoluteSlot(1000L)
            .epochNo(2)
            .slotNo(80L)
            .build();
    DRepCertificateHistoryResponse dRepCertificateHistoryResponse2 =
        DRepCertificateHistoryResponse.builder()
            .txHash(drepHash)
            .blockNo(101L)
            .createdAt(new Date(10000))
            .absoluteSlot(1000L)
            .epochNo(2)
            .slotNo(80L)
            .build();
    Pageable pageable = PageRequest.of(0, 2, Sort.by("createdAt").descending());

    when(dRepService.getTxDRepCertificateHistory(anyString(), any(Pageable.class)))
        .thenReturn(
            new BaseFilterResponse<>(
                List.of(dRepCertificateHistoryResponse1, dRepCertificateHistoryResponse2), 2L));
    mockMvc
        .perform(
            get("/api/v1/dreps/{drepHashOrDrepId}/certificates-history", drepHash)
                .param("page", "0")
                .param("size", "2")
                .param("sort", "createdAt,DESC")
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(containsString("43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e")))
        .andExpect(jsonPath("$.data[0].blockNo").value(100L));

    verify(dRepService).getTxDRepCertificateHistory(drepHash, pageable);
  }

  @Test
  void testGetTxDRepCertificatesHistory_withoutPagable() throws Exception {
    String drepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    DRepCertificateHistoryResponse dRepCertificateHistoryResponse =
        DRepCertificateHistoryResponse.builder()
            .txHash(drepHash)
            .blockNo(100L)
            .absoluteSlot(1000L)
            .epochNo(2)
            .slotNo(80L)
            .build();
    // default
    Pageable pageable = PageRequest.of(0, 20, Sort.by("createdAt").descending());

    when(dRepService.getTxDRepCertificateHistory(anyString(), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(List.of(dRepCertificateHistoryResponse), 1L));
    mockMvc
        .perform(
            get("/api/v1/dreps/{drepHashOrDrepId}/certificates-history", drepHash)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(containsString("43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e")))
        .andExpect(jsonPath("$.data[0].blockNo").value(100L));

    verify(dRepService).getTxDRepCertificateHistory(drepHash, pageable);
  }

  @Test
  void testGetDrepDetails() throws Exception {
    String drepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";

    DRepDetailsResponse dRepDetailsResponse =
        DRepDetailsResponse.builder()
            .drepId("dRepId")
            .drepHash(drepHash)
            .activeVoteStake(BigInteger.TEN)
            .delegators(10)
            .liveStake(BigInteger.TEN)
            .votingParticipation(Float.valueOf("0.5"))
            .build();

    when(dRepService.getDRepDetails(drepHash)).thenReturn(dRepDetailsResponse);
    mockMvc
        .perform(
            get("/api/v1/dreps/{drepHashOrDrepId}/drep-details", drepHash)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(containsString("43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e")))
        .andExpect(jsonPath("$.votingParticipation").value(0.5));
  }

  @Test
  void testGetChartDRepVoteOnGovernanceAction() throws Exception {
    String dRepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    VotingProcedureChartResponse votingProcedureChartResponse =
        VotingProcedureChartResponse.builder()
            .govActionType(GovActionType.INFO_ACTION)
            .dRepHash(dRepHash)
            .numberOfYesVote(10L)
            .numberOfNoVotes(0L)
            .numberOfAbstainVotes(5L)
            .build();
    when(dRepService.getVoteProcedureChart(dRepHash, GovActionType.INFO_ACTION))
        .thenReturn(votingProcedureChartResponse);
    mockMvc
        .perform(
            get("/api/v1/dreps/{dRepHash}/vote-procedure-chart", dRepHash)
                .param("govActionType", "INFO_ACTION")
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(containsString("43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e")))
        .andExpect(jsonPath("$.numberOfYesVote").value(10L));
  }

  @Test
  void testGetDetailsOfDRep() throws Exception {
    String dRepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    DRepDetailsResponse dRepDetailsResponse =
        DRepDetailsResponse.builder().drepHash(dRepHash).drepId("dRepId").delegators(100).build();
    when(dRepService.getDRepDetails(dRepHash)).thenReturn(dRepDetailsResponse);
    mockMvc
        .perform(
            get("/api/v1/dreps/{dRepHashOrDRepId}/drep-details", dRepHash)
                .param("dRepHashOrDRepId", dRepHash)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(containsString("43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e")))
        .andExpect(jsonPath("$.delegators").value(100));
  }

  @Test
  void testGetDelegation() throws Exception {
    String dRepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    DRepDelegatorsResponse dRepDelegatorsResponse =
        DRepDelegatorsResponse.builder()
            .stakeAddress("address1")
            .totalStake(BigInteger.TEN)
            .build();
    DRepDelegatorsResponse dRepDelegatorsResponse1 =
        DRepDelegatorsResponse.builder()
            .stakeAddress("address2")
            .totalStake(BigInteger.TEN)
            .build();

    when(dRepService.getDRepDelegators(any(), any()))
        .thenReturn(
            new BaseFilterResponse<>(List.of(dRepDelegatorsResponse, dRepDelegatorsResponse1), 2L));
    mockMvc
        .perform(
            get("/api/v1/dreps/{dRepHashOrDRepId}/get-delegation", dRepHash)
                .param("dRepHashOrDRepId", dRepHash)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString("address1")))
        .andExpect(jsonPath("$.data[1].stakeAddress").value("address2"));
  }
}
