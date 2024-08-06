package org.cardanofoundation.explorer.api.controller;

import static org.hamcrest.core.StringContains.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import org.cardanofoundation.explorer.api.model.request.governanceAction.VoteFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.*;
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
            .activeVoteStake(BigInteger.TEN)
            .totalYesVoteStake(BigInteger.ONE)
            .totalNoVoteStake(BigInteger.TWO)
            .abstainVoteStake(BigInteger.ZERO)
            .txHash(txHash)
            .build();

    when(governanceActionService.getVotingChartByGovActionTxHashAndIndex(anyString(), any(), any()))
        .thenReturn(votingChartResponse);
    mockMvc
        .perform(
            get("/api/v1/gov-actions/voting-chart")
                .param("txHash", txHash)
                .param("index", Integer.toString(index))
                .param("voterType", VoterType.DREP_KEY_HASH.toString())
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString(txHash)))
        .andExpect(jsonPath("$.activeVoteStake").value(BigInteger.TEN));
  }

  @Test
  void getGovActionDetails() throws Exception {
    String txHash = "61435ac83ffac3353f6845ea17133a6cc93587ae3f28528b2316cc9cbee442d7";
    int index = 0;

    GovernanceActionOverViewResponse governanceActionOverViewResponse =
        GovernanceActionOverViewResponse.builder()
            .txHash(txHash)
            .index(index)
            .actionType(GovActionType.INFO_ACTION)
            .status(GovActionStatus.ENACTED)
            .abstractContent("abstract")
            .motivation("motivation")
            .rationale("rationale")
            .isValidHash(true)
            .allowedVoteBySPO(true)
            .allowedVoteByCC(true)
            .anchorHash("1111")
            .anchorUrl("1111")
            .build();

    when(governanceActionService.getGovernanceActionInfo(anyString(), any()))
        .thenReturn(governanceActionOverViewResponse);
    mockMvc
        .perform(
            get("/api/v1/gov-actions/{txHash}/{index}", txHash, index)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString(txHash)))
        .andExpect(jsonPath("$.txHash").value(txHash))
        .andExpect(jsonPath("$.index").value(index))
        .andExpect(jsonPath("$.actionType").value("INFO_ACTION"))
        .andExpect(jsonPath("$.status").value("ENACTED"))
        .andExpect(jsonPath("$.abstract").value("abstract"))
        .andExpect(jsonPath("$.motivation").value("motivation"))
        .andExpect(jsonPath("$.rationale").value("rationale"))
        .andExpect(jsonPath("$.isValidHash").value(true))
        .andExpect(jsonPath("$.allowedVoteBySPO").value(true))
        .andExpect(jsonPath("$.allowedVoteByCC").value(true))
        .andExpect(jsonPath("$.anchorHash").value("1111"))
        .andExpect(jsonPath("$.anchorUrl").value("1111"));
  }

  @Test
  void testGetAuthorsByAnchorUrlAndAnchorHash() throws Exception {
    String anchorUrl = "1111";
    String anchorHash = "1111";

    AuthorResponse authorResponse =
        AuthorResponse.builder()
            .name("name")
            .witnessAlgorithm("witnessAlgorithm")
            .publicKey("publicKey")
            .signature("signature")
            .build();

    AuthorResponse authorResponse1 =
        AuthorResponse.builder()
            .name("zname")
            .witnessAlgorithm("witnessAlgorithm")
            .publicKey("publicKey")
            .signature("signature")
            .build();

    when(governanceActionService.getAuthorsByAnchor(anyString(), anyString(), any()))
        .thenReturn(new BaseFilterResponse<>(List.of(authorResponse, authorResponse1), 2L));

    mockMvc
        .perform(
            get("/api/v1/gov-actions/authors")
                .param("anchorUrl", anchorUrl)
                .param("anchorHash", anchorHash)
                .param("page", "0")
                .param("size", "6")
                .param("sort", "name")
                .param("direction", "ASC")
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString("name")))
        .andExpect(jsonPath("$.data[0].name").value("name"))
        .andExpect(jsonPath("$.data[0].witnessAlgorithm").value("witnessAlgorithm"))
        .andExpect(jsonPath("$.data[0].publicKey").value("publicKey"))
        .andExpect(jsonPath("$.data[0].signature").value("signature"))
        .andExpect(jsonPath("$.data[1].name").value("zname"));
  }

  @Test
  void testGetVotesOnGovAction() throws Exception {
    VoteFilter voteFilter = VoteFilter.builder().txHash("txHash").index(0).build();

    VotingOnGovActionResponse votingOnGovActionResponse = new VotingOnGovActionResponse();
    votingOnGovActionResponse.setVote(Vote.YES);
    votingOnGovActionResponse.setVoterHash("voterHash");
    votingOnGovActionResponse.setTimestamp(null);
    votingOnGovActionResponse.setVoterType(VoterType.DREP_KEY_HASH);
    votingOnGovActionResponse.setVotingStake(BigInteger.ONE);

    when(governanceActionService.getVotingOnGovAction(any(), any()))
        .thenReturn(new BaseFilterResponse<>(List.of(votingOnGovActionResponse), 1L));

    mockMvc
        .perform(
            get("/api/v1/gov-actions/votes")
                .content(objectMapper.writeValueAsString(voteFilter))
                .param("page", "0")
                .param("size", "3")
                .param("sort", "blockTime")
                .param("direction", "DESC")
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().string(containsString("voterHash")))
        .andExpect(jsonPath("$.data[0].vote").value("YES"))
        .andExpect(jsonPath("$.data[0].voterHash").value("voterHash"))
        .andExpect(jsonPath("$.data[0].voterType").value("DREP_KEY_HASH"))
        .andExpect(jsonPath("$.data[0].votingStake").value(1));
  }

  @Test
  void testGetRangeFilterForVoteSection() throws Exception {

    String txHash = "4864566ddbf27933f7c85f9ab0c58e345de0e40ce03926f5a83be3da2f928ffe";
    Integer index = 0;

    RangeFilterVoteResponse response = new RangeFilterVoteResponse();

    response.setMaxActiveStake(BigInteger.TEN);
    response.setMinActiveStake(BigInteger.ONE);
    when(governanceActionService.getRangeFilterVoteResponse(txHash, index)).thenReturn(response);
    mockMvc
        .perform(
            get("/api/v1/gov-actions/{txHash}/{index}/votes/range-values", txHash, index)
                .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.maxActiveStake").value(10))
        .andExpect(jsonPath("$.minActiveStake").value(1));
  }

  @Test
  void testGetGovActionCommitteeHistoryByFilter() throws Exception {

    Map<GovActionType, Long> govCountMap = new HashMap<>();
    govCountMap.put(GovActionType.INFO_ACTION, 10L);
    govCountMap.put(GovActionType.NO_CONFIDENCE, 20L);
    govCountMap.put(GovActionType.PARAMETER_CHANGE_ACTION, 30L);
    govCountMap.put(GovActionType.TREASURY_WITHDRAWALS_ACTION, 40L);
    govCountMap.put(GovActionType.UPDATE_COMMITTEE, 50L);
    govCountMap.put(GovActionType.HARD_FORK_INITIATION_ACTION, 60L);
    govCountMap.put(GovActionType.NEW_CONSTITUTION, 70L);
    Map<GovActionStatus, Long> govStatusMap = new HashMap<>();
    govStatusMap.put(GovActionStatus.ENACTED, 10L);
    govStatusMap.put(GovActionStatus.EXPIRED, 20L);
    govStatusMap.put(GovActionStatus.RATIFIED, 30L);
    govStatusMap.put(GovActionStatus.OPEN_BALLOT, 40L);

    GovernanceOverviewResponse response =
        GovernanceOverviewResponse.builder()
            .activeCommittees(10L)
            .activeDReps(10L)
            .activeSPOs(10L)
            .totalGovActions(10L)
            .govCountMap(govCountMap)
            .govStatusMap(govStatusMap)
            .build();
    when(governanceActionService.getGovernanceOverview()).thenReturn(response);
    mockMvc
        .perform(get("/api/v1/gov-actions/overview").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.activeCommittees").value(10))
        .andExpect(jsonPath("$.activeDReps").value(10))
        .andExpect(jsonPath("$.activeSPOs").value(10))
        .andExpect(jsonPath("$.totalGovActions").value(10))
        .andExpect(jsonPath("$.govCountMap.INFO_ACTION").value(10))
        .andExpect(jsonPath("$.govCountMap.NO_CONFIDENCE").value(20))
        .andExpect(jsonPath("$.govCountMap.PARAMETER_CHANGE_ACTION").value(30))
        .andExpect(jsonPath("$.govCountMap.TREASURY_WITHDRAWALS_ACTION").value(40))
        .andExpect(jsonPath("$.govCountMap.UPDATE_COMMITTEE").value(50))
        .andExpect(jsonPath("$.govCountMap.HARD_FORK_INITIATION_ACTION").value(60))
        .andExpect(jsonPath("$.govCountMap.NEW_CONSTITUTION").value(70))
        .andExpect(jsonPath("$.govStatusMap.ENACTED").value(10))
        .andExpect(jsonPath("$.govStatusMap.EXPIRED").value(20))
        .andExpect(jsonPath("$.govStatusMap.RATIFIED").value(30))
        .andExpect(jsonPath("$.govStatusMap.OPEN_BALLOT").value(40));
  }
}
