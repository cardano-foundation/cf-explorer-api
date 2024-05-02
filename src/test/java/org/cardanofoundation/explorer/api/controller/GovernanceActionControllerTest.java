package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@WebMvcTest(GovernanceActionController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class GovernanceActionControllerTest {
  @MockBean private AuthInterceptor authInterceptor;
  @Autowired private MockMvc mockMvc;
  @MockBean private GovernanceActionService governanceActionService;
  @Autowired private ObjectMapper objectMapper;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetGovActionByFilter() throws Exception {
    String dRepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    GovernanceActionFilter filter =
        GovernanceActionFilter.builder()
            .actionStatus(GovActionStatus.ANY)
            .voterType(VoterType.DREP_KEY_HASH)
            .actionType(GovActionType.ALL)
            .voteType(Vote.ANY)
            .build();

    GovernanceActionResponse governanceActionResponse =
        GovernanceActionResponse.builder()
            .txHash("61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7")
            .status(GovActionStatus.ENACTED)
            .index(0L)
            .vote(Vote.YES)
            .build();

    when(governanceActionService.getGovernanceActions(anyString(), any(), any()))
        .thenReturn(new BaseFilterResponse<>(List.of(governanceActionResponse), 1L));
    mockMvc
        .perform(
            get("/api/v1/gov-actions/{dRepHashOrPoolHash}", dRepHash)
                .param("page", "0")
                .param("size", "20")
                .param("sort", "blockTime,desc")
                .content(objectMapper.writeValueAsString(filter))
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7")))
        .andExpect(jsonPath("$.data[0].status").value(GovActionStatus.ENACTED.toString()));
  }

  @Test
  void testGetVotingProcedureChart() throws Exception {
    String dRepHash = "43a0e2e2d6bf1d0c48b0eb1744fb853407c6b94f2de79f0508c5962e";
    GovernanceActionRequest governanceActionRequest =
        GovernanceActionRequest.builder()
            .txHash("61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7")
            .index(0)
            .voterType(VoterType.DREP_KEY_HASH)
            .build();

    HistoryVote historyVote = HistoryVote.builder().vote(Vote.YES).build();
    HistoryVote historyVote1 = HistoryVote.builder().vote(Vote.NO).build();

    GovernanceActionDetailsResponse governanceActionDetailsResponse =
        GovernanceActionDetailsResponse.builder()
            .txHash("61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7")
            .index(0L)
            .anchorHash("1111")
            .anchorUrl("1111")
            .historyVotes(List.of(historyVote, historyVote1))
            .build();

    when(governanceActionService.getGovernanceActionDetails(anyString(), any()))
        .thenReturn(governanceActionDetailsResponse);
    mockMvc
        .perform(
            get("/api/v1/gov-actions/{dRepHashOrPoolHash}/voting-procedure-detail", dRepHash)
                .content(objectMapper.writeValueAsString(governanceActionRequest))
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            content()
                .string(
                    containsString(
                        "61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7")))
        .andExpect(jsonPath("$.anchorHash").value("1111"));
  }

  @Test
  void getVotingChartByGovAction() throws Exception {
    String txHash = "61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7";
    int index = 0;

    VotingChartResponse votingChartResponse =
        VotingChartResponse.builder()
            .numberOfNoVotes(0L)
            .numberOfYesVote(1L)
            .numberOfAbstainVotes(2L)
            .txHash(txHash)
            .index(index)
            .build();

    when(governanceActionService.getVotingChartByGovActionTxHashAndIndex(anyString(), any()))
        .thenReturn(votingChartResponse);
    mockMvc
        .perform(
            get("/api/v1/gov-actions/voting-chart")
                .param("txHash", txHash)
                .param("index", Integer.toString(index))
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString(txHash)))
        .andExpect(jsonPath("$.numberOfYesVote").value(1L));
  }
}
