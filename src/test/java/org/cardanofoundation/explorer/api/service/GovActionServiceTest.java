package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mapstruct.factory.Mappers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.mapper.GovernanceActionMapper;
import org.cardanofoundation.explorer.api.mapper.LatestVotingProcedureMapper;
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.request.governanceAction.VoteFilter;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.StakeAddressBalanceRepository;
import org.cardanofoundation.explorer.api.service.impl.GovernanceActionServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.ledgersync.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;

@ExtendWith(MockitoExtension.class)
public class GovActionServiceTest {

  @InjectMocks GovernanceActionServiceImpl governanceActionService;

  @Mock DRepRegistrationRepository dRepRegistrationRepository;
  @Mock GovernanceActionRepository governanceActionRepository;
  @Mock VotingProcedureRepository votingProcedureRepository;
  @Mock LatestVotingProcedureRepository latestVotingProcedureRepository;
  @Mock PoolHashRepository poolHashRepository;
  @Mock DrepInfoRepository drepInfoRepository;
  @Mock EpochParamRepository epochParamRepository;
  @Mock CommitteeRegistrationRepository committeeRegistrationRepository;
  @Mock CommitteeMemberRepository committeeMemberRepository;
  @Mock DelegationRepository delegationRepository;
  @Mock StakeAddressBalanceRepository stakeAddressBalanceRepository;
  @Mock OffChainVoteGovActionDataRepository offChainVoteGovActionDataRepository;
  @Mock CommitteeRepository committeeRepository;
  @Mock ProtocolParamService protocolParamService;
  @Mock RedisTemplate<String, Integer> redisTemplate;

  @Mock ValueOperations valueOperations;

  @Spy
  private GovernanceActionMapper governanceActionMapper =
      Mappers.getMapper(GovernanceActionMapper.class);

  @Spy
  private VotingProcedureMapper votingProcedureMapper =
      Mappers.getMapper(VotingProcedureMapper.class);

  @Spy
  LatestVotingProcedureMapper latestVotingProcedureMapper =
      Mappers.getMapper(LatestVotingProcedureMapper.class);

  @Mock private ObjectMapper objectMapper;

  @Test
  void testGetGovernanceActions() {
    String dRepHash = "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758";
    String dRepId = "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4s6tcwcy";

    DRepInfo dRepInfo = DRepInfo.builder().drepHash(dRepHash).build();
    when(drepInfoRepository.findByDRepHashOrDRepId(dRepId)).thenReturn(Optional.of(dRepInfo));

    Pageable pageable = PageRequest.of(0, 10, Sort.by("blockTime").descending());
    GovernanceActionFilter governanceActionFilter =
        GovernanceActionFilter.builder()
            .voterType(VoterType.DREP_KEY_HASH)
            .voteType(Vote.ANY)
            .actionStatus(GovActionStatus.ANY)
            .actionType(GovActionType.ALL)
            .build();
    Long slot = 0L;
    when(dRepRegistrationRepository.getSlotOfDRepRegistration(dRepHash)).thenReturn(slot);

    GovernanceActionProjection projection1 = Mockito.mock(GovernanceActionProjection.class);
    when(projection1.getTxHash()).thenReturn("hash1");
    when(projection1.getIndex()).thenReturn(0);
    when(projection1.getVote()).thenReturn(Vote.YES);
    when(projection1.getRepeatVote()).thenReturn(false);
    when(projection1.getType()).thenReturn(GovActionType.INFO_ACTION);

    GovernanceActionProjection projection3 = Mockito.mock(GovernanceActionProjection.class);
    when(projection3.getTxHash()).thenReturn("hash3");
    when(projection3.getIndex()).thenReturn(0);
    when(projection3.getVote()).thenReturn(Vote.YES);
    when(governanceActionRepository.getAllByFilter(
            any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(projection1, projection3), pageable, 2));

    var actual =
        governanceActionService.getGovernanceActions(dRepId, governanceActionFilter, pageable);

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("hash1", actual.getData().get(0).getTxHash());
  }

  @Test
  void testGetGovernanceActions_withVoterIsPool() {
    String poolHash = "36afbddfd460c06b4856ff5895c8134a8421d3817721e220662bd237";
    String poolView = "pool1x6hmmh75vrqxkjzklavftjqnf2zzr5upwus7ygrx90frwmrz69e";

    when(poolHashRepository.getHashRawByView(poolView)).thenReturn(Optional.of(poolHash));

    Pageable pageable = PageRequest.of(0, 10, Sort.by("blockTime").descending());
    GovernanceActionFilter governanceActionFilter =
        GovernanceActionFilter.builder()
            .voterType(VoterType.STAKING_POOL_KEY_HASH)
            .voteType(Vote.ANY)
            .actionStatus(GovActionStatus.ANY)
            .actionType(GovActionType.INFO_ACTION)
            .build();
    Long slot = 0L;
    when(poolHashRepository.getSlotNoWhenFirstDelegationByPoolHash(poolHash)).thenReturn(slot);

    GovernanceActionProjection projection1 = Mockito.mock(GovernanceActionProjection.class);
    when(projection1.getTxHash()).thenReturn("hash1");
    when(projection1.getIndex()).thenReturn(0);
    when(projection1.getVote()).thenReturn(Vote.NO);
    when(projection1.getRepeatVote()).thenReturn(false);
    when(projection1.getType()).thenReturn(GovActionType.INFO_ACTION);
    when(governanceActionRepository.getAllByFilter(
            any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(projection1), pageable, 1));

    var actual =
        governanceActionService.getGovernanceActions(poolView, governanceActionFilter, pageable);

    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals("hash1", actual.getData().get(0).getTxHash());
  }

  @Test
  void getGovActionDetails() {
    String txHash = "358d960c2c18a845272d055f0d36c41dd3e869c8cf811ab12acac27f56970420";
    Integer index = 0;
    String voterHash = "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758";
    String dRepId = "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4s6tcwcy";

    when(drepInfoRepository.findByDRepHashOrDRepId(dRepId))
        .thenReturn(Optional.of(DRepInfo.builder().drepHash(voterHash).build()));
    VoterType voterType = VoterType.DREP_KEY_HASH;
    GovernanceActionRequest governanceActionRequest =
        GovernanceActionRequest.builder().voterType(voterType).txHash(txHash).index(index).build();

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);

    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.INFO_ACTION);
    when(govActionDetailsProjection.getAnchorHash()).thenReturn("anchorHash");
    when(govActionDetailsProjection.getEpoch()).thenReturn(10);

    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    VotingProcedureProjection votingProcedureProjection =
        Mockito.mock(VotingProcedureProjection.class);
    when(votingProcedureProjection.getVote()).thenReturn(Vote.YES);
    when(votingProcedureProjection.getBlockTime()).thenReturn(0L);

    VotingProcedureProjection votingProcedureProjection1 =
        Mockito.mock(VotingProcedureProjection.class);
    when(votingProcedureProjection1.getVote()).thenReturn(Vote.NO);
    when(votingProcedureProjection1.getBlockTime()).thenReturn(1707695293L);

    when(votingProcedureRepository.getVotingProcedureByTxHashAndIndexAndVoterHash(
            txHash, index, voterHash, List.of(VoterType.DREP_KEY_HASH, VoterType.DREP_SCRIPT_HASH)))
        .thenReturn(List.of(votingProcedureProjection1, votingProcedureProjection));

    EpochParam epochParam =
        EpochParam.builder().epochNo(10).govActionLifetime(BigInteger.TEN).build();

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);

    var actual =
        governanceActionService.getGovernanceActionDetails(dRepId, governanceActionRequest);

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(txHash, actual.getTxHash());
    Assertions.assertEquals(Vote.NO, actual.getVoteType());
  }

  @Test
  void getGovActionDetails_withPoolView() {
    String txHash = "fb8c44de0a6c4d6e1826ad325b9fb208d97d756345d5803c8d4682a1ecee0efb";
    Integer index = 0;
    String voterHash = "36afbddfd460c06b4856ff5895c8134a8421d3817721e220662bd237";
    String poolView = "pool1x6hmmh75vrqxkjzklavftjqnf2zzr5upwus7ygrx90frwmrz69e";

    when(poolHashRepository.getHashRawByView(poolView)).thenReturn(Optional.of(voterHash));
    VoterType voterType = VoterType.DREP_KEY_HASH;
    GovernanceActionRequest governanceActionRequest =
        GovernanceActionRequest.builder().voterType(voterType).txHash(txHash).index(index).build();

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);

    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.INFO_ACTION);
    when(govActionDetailsProjection.getAnchorHash()).thenReturn("anchorHash");
    when(govActionDetailsProjection.getEpoch()).thenReturn(10);

    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    VotingProcedureProjection votingProcedureProjection =
        Mockito.mock(VotingProcedureProjection.class);
    when(votingProcedureProjection.getVote()).thenReturn(Vote.YES);
    when(votingProcedureProjection.getBlockTime()).thenReturn(0L);

    VotingProcedureProjection votingProcedureProjection1 =
        Mockito.mock(VotingProcedureProjection.class);
    when(votingProcedureProjection1.getVote()).thenReturn(Vote.NO);
    when(votingProcedureProjection1.getBlockTime()).thenReturn(1707695293L);

    when(votingProcedureRepository.getVotingProcedureByTxHashAndIndexAndVoterHash(
            txHash, index, voterHash, List.of(VoterType.DREP_KEY_HASH, VoterType.DREP_SCRIPT_HASH)))
        .thenReturn(List.of(votingProcedureProjection1, votingProcedureProjection));

    EpochParam epochParam =
        EpochParam.builder().epochNo(10).govActionLifetime(BigInteger.TEN).build();

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);

    var actual =
        governanceActionService.getGovernanceActionDetails(poolView, governanceActionRequest);

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(txHash, actual.getTxHash());
    Assertions.assertEquals(Vote.NO, actual.getVoteType());
  }

  @Test
  void testGetGovCommitteeStatusHistory() {
    GovCommitteeHistoryFilter filter =
        GovCommitteeHistoryFilter.builder().actionType(GovActionType.ALL).build();

    GovernanceActionProjection projection = Mockito.mock(GovernanceActionProjection.class);
    when(projection.getTxHash()).thenReturn("hash1");
    when(projection.getIndex()).thenReturn(0);
    when(projection.getType()).thenReturn(GovActionType.INFO_ACTION);

    GovernanceActionProjection projection1 = Mockito.mock(GovernanceActionProjection.class);
    when(projection1.getTxHash()).thenReturn("hash2");
    when(projection1.getIndex()).thenReturn(0);
    when(projection1.getType()).thenReturn(GovActionType.INFO_ACTION);

    when(governanceActionRepository.getAllGovCommitteeHistory(
            any(), any(), any(), any(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(projection, projection1)));

    var actual =
        governanceActionService.getGovCommitteeStatusHistory(filter, PageRequest.of(0, 10));

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("hash1", actual.getData().get(0).getTxHash());
    Assertions.assertEquals("hash2", actual.getData().get(1).getTxHash());
  }

  @Test
  void testGetGovernanceOverview() {
    EpochParam epochParam = EpochParam.builder().build();
    when(epochParamRepository.findCurrentEpochParam()).thenReturn(epochParam);

    when(drepInfoRepository.countByStatus(any())).thenReturn(5L);

    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    when(valueOperations.get(any())).thenReturn(5);
    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(any()))
        .thenReturn(5L);
    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(any()))
        .thenReturn(3L);

    GovernanceActionProjection projection = Mockito.mock(GovernanceActionProjection.class);
    when(projection.getType()).thenReturn(GovActionType.INFO_ACTION);
    when(projection.getGovCount()).thenReturn(5L);

    GovernanceActionProjection projection1 = Mockito.mock(GovernanceActionProjection.class);
    when(projection1.getType()).thenReturn(GovActionType.PARAMETER_CHANGE_ACTION);
    when(projection1.getGovCount()).thenReturn(3L);

    GovernanceActionProjection projection2 = Mockito.mock(GovernanceActionProjection.class);
    when(projection2.getType()).thenReturn(GovActionType.UPDATE_COMMITTEE);
    when(projection2.getGovCount()).thenReturn(2L);

    when(governanceActionRepository.getGovActionGroupByGovActionType())
        .thenReturn(List.of(projection, projection1, projection2));

    GovernanceActionProjection projection3 = Mockito.mock(GovernanceActionProjection.class);
    when(projection3.getStatus()).thenReturn(GovActionStatus.OPEN_BALLOT);
    when(projection3.getGovCount()).thenReturn(5L);

    GovernanceActionProjection projection4 = Mockito.mock(GovernanceActionProjection.class);
    when(projection4.getStatus()).thenReturn(GovActionStatus.ENACTED);
    when(projection4.getGovCount()).thenReturn(6L);

    GovernanceActionProjection projection5 = Mockito.mock(GovernanceActionProjection.class);
    when(projection5.getStatus()).thenReturn(GovActionStatus.EXPIRED);
    when(projection5.getGovCount()).thenReturn(7L);

    when(governanceActionRepository.getGovActionGroupByGovActionStatus())
        .thenReturn(List.of(projection3, projection4, projection5));

    var actual = governanceActionService.getGovernanceOverview();

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(5, actual.getActiveDReps());
    Assertions.assertEquals(3, actual.getActiveCommittees());
    Assertions.assertEquals(10, actual.getTotalGovActions());
    Assertions.assertEquals(5, actual.getActiveSPOs());
    Assertions.assertEquals(5, actual.getGovCountMap().get(GovActionType.INFO_ACTION));
    Assertions.assertEquals(3, actual.getGovCountMap().get(GovActionType.PARAMETER_CHANGE_ACTION));
    Assertions.assertEquals(2, actual.getGovCountMap().get(GovActionType.UPDATE_COMMITTEE));
    Assertions.assertEquals(null, actual.getGovCountMap().get(GovActionType.NO_CONFIDENCE));
    Assertions.assertEquals(5, actual.getGovStatusMap().get(GovActionStatus.OPEN_BALLOT));
    Assertions.assertEquals(6, actual.getGovStatusMap().get(GovActionStatus.ENACTED));
    Assertions.assertEquals(7, actual.getGovStatusMap().get(GovActionStatus.EXPIRED));
    Assertions.assertEquals(null, actual.getGovStatusMap().get(GovActionStatus.RATIFIED));
  }

  @Test
  void testGetVotingChartByGovActionTxHashAndIndex() {
    String txHash = "4b5a77c3289a41f104e4a4c1fe82319fb047ce12f3d42ff2837caa2b0f2e93de";
    Integer index = 0;
    VoterType voterType = VoterType.DREP_KEY_HASH;

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);
    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getEpoch()).thenReturn(5);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.UPDATE_COMMITTEE);

    EpochParam epochParam =
        EpochParam.builder()
            .committeeMinSize(BigInteger.valueOf(5))
            .dvtCommitteeNormal(0.51)
            .govActionLifetime(BigInteger.TEN)
            .epochNo(200)
            .build();

    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);
    when(epochParamRepository.findCurrentEpochParam()).thenReturn(epochParam);

    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(200)).thenReturn(5L);

    DRepInfoProjection projection1 = Mockito.mock(DRepInfoProjection.class);
    when(projection1.getDrepHash()).thenReturn("drep1");
    when(projection1.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    DRepInfoProjection projection2 = Mockito.mock(DRepInfoProjection.class);
    when(projection2.getDrepHash()).thenReturn("drep2");
    when(projection2.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    DRepInfoProjection projection3 = Mockito.mock(DRepInfoProjection.class);
    when(projection3.getDrepHash()).thenReturn("drep3");
    when(projection3.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    when(drepInfoRepository.findDRepByCreatedAt(any()))
        .thenReturn(List.of(projection1, projection2, projection3));

    LatestVotingProcedureProjection prj1 = Mockito.mock(LatestVotingProcedureProjection.class);
    when(prj1.getVoterHash()).thenReturn("drep1");
    when(prj1.getVote()).thenReturn(Vote.YES);

    LatestVotingProcedureProjection prj2 = Mockito.mock(LatestVotingProcedureProjection.class);
    when(prj2.getVoterHash()).thenReturn("drep2");
    when(prj2.getVote()).thenReturn(Vote.NO);

    when(latestVotingProcedureRepository.findByGovActionTxHashAndGovActionIndex(
            any(), any(), any(), any()))
        .thenReturn(List.of(prj1, prj2));

    var votingChartResponse =
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index, voterType);

    Assertions.assertNotNull(votingChartResponse);
    Assertions.assertEquals(BigInteger.valueOf(100), votingChartResponse.getTotalYesVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(100), votingChartResponse.getTotalNoVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(300), votingChartResponse.getActiveVoteStake());
    Assertions.assertEquals(Double.valueOf(0.51), votingChartResponse.getThreshold());
  }

  @Test
  void testGetVotingChartByGovActionTxHashAndIndex_withSPOType() {
    String txHash = "4b5a77c3289a41f104e4a4c1fe82319fb047ce12f3d42ff2837caa2b0f2e93de";
    Integer index = 0;
    VoterType voterType = VoterType.STAKING_POOL_KEY_HASH;

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);
    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getEpoch()).thenReturn(5);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.INFO_ACTION);

    EpochParam epochParam =
        EpochParam.builder()
            .committeeMinSize(BigInteger.valueOf(5))
            .dvtCommitteeNormal(0.51)
            .govActionLifetime(BigInteger.TEN)
            .epochNo(200)
            .build();

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);
    when(epochParamRepository.findCurrentEpochParam()).thenReturn(epochParam);

    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(200)).thenReturn(5L);

    PoolOverviewProjection projection1 = Mockito.mock(PoolOverviewProjection.class);
    when(projection1.getPoolHash()).thenReturn("pool1");
    when(projection1.getPoolId()).thenReturn(1L);

    PoolOverviewProjection projection2 = Mockito.mock(PoolOverviewProjection.class);
    when(projection2.getPoolHash()).thenReturn("pool2");
    when(projection2.getPoolId()).thenReturn(2L);

    when(poolHashRepository.getSlotCreatedAtGroupByPoolHash(any()))
        .thenReturn(List.of(projection1, projection2));

    LatestVotingProcedureProjection latestVotingProcedureProjection =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(latestVotingProcedureProjection.getVoterHash()).thenReturn("pool1");
    when(latestVotingProcedureProjection.getVote()).thenReturn(Vote.YES);

    LatestVotingProcedureProjection latestVotingProcedureProjection1 =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(latestVotingProcedureProjection1.getVoterHash()).thenReturn("pool2");
    when(latestVotingProcedureProjection1.getVote()).thenReturn(Vote.NO);

    when(delegationRepository.getStakeAddressDelegatorsByPoolIds(any()))
        .thenReturn(Set.of("view1", "view2", "view3"));
    when(stakeAddressBalanceRepository.sumBalanceByStakeAddressIn(any()))
        .thenReturn(BigInteger.valueOf(110));

    when(latestVotingProcedureRepository.findByGovActionTxHashAndGovActionIndex(
            any(), any(), any(), any()))
        .thenReturn(List.of(latestVotingProcedureProjection, latestVotingProcedureProjection1));

    var actual =
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index, voterType);

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(BigInteger.valueOf(110), actual.getActiveVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(110), actual.getTotalYesVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(110), actual.getTotalNoVoteStake());
    Assertions.assertEquals(BigInteger.ZERO, actual.getAbstainVoteStake());
    Assertions.assertEquals(1.0, actual.getThreshold());
  }

  @Test
  void testGetVotingChartByGovActionTxHashAndIndex_withDRepType() throws JsonProcessingException {
    String txHash = "4b5a77c3289a41f104e4a4c1fe82319fb047ce12f3d42ff2837caa2b0f2e93de";
    Integer index = 0;
    VoterType voterType = VoterType.DREP_KEY_HASH;

    JsonNode node = getMetaDataOfGovAction();

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);
    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getEpoch()).thenReturn(5);
    when(govActionDetailsProjection.getDetails()).thenReturn(node);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.PARAMETER_CHANGE_ACTION);

    EpochParam epochParam =
        EpochParam.builder()
            .dvtPPEconomicGroup(0.5)
            .dvtPPNetworkGroup(0.6)
            .dvtPPTechnicalGroup(0.7)
            .dvtPPGovGroup(0.8)
            .committeeMinSize(BigInteger.valueOf(8))
            .epochNo(200)
            .build();

    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);
    when(epochParamRepository.findCurrentEpochParam()).thenReturn(epochParam);

    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(200)).thenReturn(5L);

    DRepInfoProjection projection1 = Mockito.mock(DRepInfoProjection.class);
    when(projection1.getDrepHash()).thenReturn("drep1");
    when(projection1.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    DRepInfoProjection projection2 = Mockito.mock(DRepInfoProjection.class);
    when(projection2.getDrepHash()).thenReturn("drep2");
    when(projection2.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    DRepInfoProjection projection3 = Mockito.mock(DRepInfoProjection.class);
    when(projection3.getDrepHash()).thenReturn("drep3");
    when(projection3.getActiveVoteStake()).thenReturn(BigInteger.valueOf(100));

    when(drepInfoRepository.findDRepByCreatedAt(any()))
        .thenReturn(List.of(projection1, projection2, projection3));

    LatestVotingProcedureProjection prj1 = Mockito.mock(LatestVotingProcedureProjection.class);
    when(prj1.getVoterHash()).thenReturn("drep1");
    when(prj1.getVote()).thenReturn(Vote.YES);

    LatestVotingProcedureProjection prj2 = Mockito.mock(LatestVotingProcedureProjection.class);
    when(prj2.getVoterHash()).thenReturn("drep2");
    when(prj2.getVote()).thenReturn(Vote.NO);

    LatestVotingProcedureProjection prj3 = Mockito.mock(LatestVotingProcedureProjection.class);
    when(prj3.getVoterHash()).thenReturn("drep3");
    when(prj3.getVote()).thenReturn(Vote.NO);

    when(latestVotingProcedureRepository.findByGovActionTxHashAndGovActionIndex(
            any(), any(), any(), any()))
        .thenReturn(List.of(prj1, prj2, prj3));

    var votingChartResponse =
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index, voterType);

    Assertions.assertNotNull(votingChartResponse);
    Assertions.assertEquals(BigInteger.valueOf(100), votingChartResponse.getTotalYesVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(200), votingChartResponse.getTotalNoVoteStake());
    Assertions.assertEquals(BigInteger.valueOf(300), votingChartResponse.getActiveVoteStake());
    Assertions.assertEquals(Double.valueOf(0.8), votingChartResponse.getThreshold());
  }

  private JsonNode getMetaDataOfGovAction() throws JsonProcessingException {
    String jsonString =
        "{\"type\": \"PARAMETER_CHANGE_ACTION\", \"policyHash\": \"edcd84c10e36ae810dc50847477083069db796219b39ccde790484e0\", \"govActionId\": null, \"protocolParamUpdate\": {\"nopt\": null, \"minFeeA\": null, \"minFeeB\": null, \"minUtxo\": null, \"maxEpoch\": null, \"priceMem\": null, \"maxTxSize\": null, \"priceStep\": null, \"costModels\": null, \"keyDeposit\": 1000000, \"maxTxExMem\": null, \"maxValSize\": null, \"drepDeposit\": null, \"minPoolCost\": null, \"poolDeposit\": null, \"drepActivity\": null, \"extraEntropy\": null, \"maxBlockSize\": 10, \"maxTxExSteps\": null, \"expansionRate\": null, \"maxBlockExMem\": null, \"adaPerUtxoByte\": null, \"costModelsHash\": null, \"maxBlockExSteps\": null, \"committeeMinSize\": null, \"govActionDeposit\": null, \"protocolMajorVer\": null, \"protocolMinorVer\": null, \"collateralPercent\": null, \"govActionLifetime\": null, \"maxBlockHeaderSize\": null, \"treasuryGrowthRate\": null, \"maxCollateralInputs\": null, \"poolPledgeInfluence\": 0.5, \"drepVotingThresholds\": 0.5, \"poolVotingThresholds\": null, \"decentralisationParam\": null, \"committeeMaxTermLength\": null, \"minFeeRefScriptCostPerByte\": null}}";
    ObjectMapper mapper = new ObjectMapper();
    return mapper.readTree(jsonString);
  }

  @Test
  void testGetVotingChartByGovActionTxHashAndIndex_withCCType() {
    String txHash = "4b5a77c3289a41f104e4a4c1fe82319fb047ce12f3d42ff2837caa2b0f2e93de";
    Integer index = 0;
    VoterType voterType = VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH;

    GovActionDetailsProjection govActionDetailsProjection =
        Mockito.mock(GovActionDetailsProjection.class);
    when(govActionDetailsProjection.getTxHash()).thenReturn(txHash);
    when(govActionDetailsProjection.getIndex()).thenReturn(index);
    when(govActionDetailsProjection.getEpoch()).thenReturn(5);
    when(govActionDetailsProjection.getType()).thenReturn(GovActionType.PARAMETER_CHANGE_ACTION);

    EpochParam epochParam =
        EpochParam.builder().epochNo(200).committeeMinSize(BigInteger.TEN).build();

    when(committeeRepository.getLatestCCThreshold()).thenReturn(Optional.of(0.77));
    when(governanceActionRepository.getGovActionDetailsByTxHashAndIndex(txHash, index))
        .thenReturn(Optional.of(govActionDetailsProjection));

    when(epochParamRepository.findByEpochNo(any())).thenReturn(epochParam);
    when(epochParamRepository.findCurrentEpochParam()).thenReturn(epochParam);

    when(committeeMemberRepository.countActiveMembersByExpiredEpochGreaterThan(200)).thenReturn(5L);

    LatestVotingProcedureProjection projection =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(projection.getVote()).thenReturn(Vote.YES);

    LatestVotingProcedureProjection projection1 =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(projection1.getVote()).thenReturn(Vote.NO);

    LatestVotingProcedureProjection projection2 =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(projection2.getVote()).thenReturn(Vote.ABSTAIN);

    when(latestVotingProcedureRepository.getLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
            any(), any(), any(), any()))
        .thenReturn(List.of(projection, projection1, projection2));

    var actual =
        governanceActionService.getVotingChartByGovActionTxHashAndIndex(txHash, index, voterType);

    Assertions.assertNotNull(actual);
    Assertions.assertEquals(1L, actual.getYesCcMembers());
    Assertions.assertEquals(1L, actual.getNoCcMembers());
    Assertions.assertEquals(1L, actual.getAbstainCcMembers());
    Assertions.assertEquals(5L, actual.getCcMembers());
  }

  @Test
  void testGetGovActionInfo_withValidHash() {
    String txHash = "adaf41feb340c85304864e9d186a35d0ee33acdd897f73aac47135d801d0a9f4";
    Integer index = 0;
    GovernanceActionOverviewProjection projection =
        Mockito.mock(GovernanceActionOverviewProjection.class);

    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getIndex()).thenReturn(index);
    when(projection.getAbstract()).thenReturn("abstract");
    when(projection.getMotivation()).thenReturn("motivation");
    when(projection.getRationale()).thenReturn("rationale");
    when(projection.getRawData()).thenReturn("rawData");
    when(projection.getAnchorHash())
        .thenReturn("ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81");
    when(projection.getAnchorUrl())
        .thenReturn("https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json");
    when(projection.getRawData()).thenReturn(getRawData());
    when(projection.getDateCreated()).thenReturn(1720748189L);
    when(projection.getActionType()).thenReturn(GovActionType.HARD_FORK_INITIATION_ACTION);

    when(governanceActionRepository.getGovernanceActionOverviewByTxHashAndIndex(anyString(), any()))
        .thenReturn(projection);

    var actual = governanceActionService.getGovernanceActionOverviewResponse(txHash, index);

    Assertions.assertEquals(txHash, actual.getTxHash());
    Assertions.assertEquals("abstract", actual.getAbstractContent());
    Assertions.assertEquals("motivation", actual.getMotivation());
    Assertions.assertEquals("rationale", actual.getRationale());
    Assertions.assertEquals(
        "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81", actual.getAnchorHash());
    Assertions.assertEquals(
        "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json",
        actual.getAnchorUrl());
    Assertions.assertEquals(index, actual.getIndex());
    Assertions.assertTrue(actual.getIsValidHash());
    Assertions.assertEquals(1720748189L * 1000, actual.getDateCreated().getTime());
    Assertions.assertTrue(actual.getAllowedVoteByCC());
    Assertions.assertTrue(actual.getAllowedVoteBySPO());
    Assertions.assertEquals(GovActionType.HARD_FORK_INITIATION_ACTION, actual.getActionType());

    verify(governanceActionRepository).getGovernanceActionOverviewByTxHashAndIndex(txHash, index);
  }

  @Test
  void testGetGovActionInfo_withInValidHash() {
    String txHash = "5764095e8a8a86bbfa61b8c93267243a4993a680452a63e3458cb3eaa29ced72";
    Integer index = 0;
    String anchorUrl =
        "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0108/examples/treasury-withdrawal.jsonld";
    String anchorHash = "21dd5a8219936e0d756f44f7f1a7179806b5afa45b6cbfb9e7d7efe3123c8e51";
    GovernanceActionOverviewProjection projection =
        Mockito.mock(GovernanceActionOverviewProjection.class);

    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getIndex()).thenReturn(index);
    when(projection.getAbstract()).thenReturn("abstract");
    when(projection.getMotivation()).thenReturn("motivation");
    when(projection.getRationale()).thenReturn("rationale");
    when(projection.getRawData()).thenReturn("rawData");
    when(projection.getAnchorHash()).thenReturn(anchorHash);
    when(projection.getAnchorUrl()).thenReturn(anchorUrl);
    when(projection.getRawData()).thenReturn(getRawData1());
    when(projection.getDateCreated()).thenReturn(1721193668L);
    when(projection.getActionType()).thenReturn(GovActionType.INFO_ACTION);

    when(governanceActionRepository.getGovernanceActionOverviewByTxHashAndIndex(anyString(), any()))
        .thenReturn(projection);

    var actual = governanceActionService.getGovernanceActionOverviewResponse(txHash, index);

    Assertions.assertEquals(txHash, actual.getTxHash());
    Assertions.assertEquals("abstract", actual.getAbstractContent());
    Assertions.assertEquals("motivation", actual.getMotivation());
    Assertions.assertEquals("rationale", actual.getRationale());
    Assertions.assertEquals(anchorHash, actual.getAnchorHash());
    Assertions.assertEquals(anchorUrl, actual.getAnchorUrl());
    Assertions.assertEquals(index, actual.getIndex());
    Assertions.assertFalse(actual.getIsValidHash());
    Assertions.assertEquals(1721193668L * 1000, actual.getDateCreated().getTime());
    Assertions.assertTrue(actual.getAllowedVoteByCC());
    Assertions.assertTrue(actual.getAllowedVoteBySPO());
    Assertions.assertEquals(GovActionType.INFO_ACTION, actual.getActionType());

    verify(governanceActionRepository).getGovernanceActionOverviewByTxHashAndIndex(txHash, index);
  }

  @Test
  public void testGetAuthorsByAnchor() {
    String anchorUrl = "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json";
    String anchorHash = "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn(getRawData());

    var actual =
        governanceActionService.getAuthorsByAnchor(
            "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json",
            "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81",
            PageRequest.of(0, 10));
    verify(offChainVoteGovActionDataRepository)
        .getRawDataByAnchorUrlAndAnchorHash(anchorUrl, anchorHash);
    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals("Carlos", actual.getData().get(0).getName());
    Assertions.assertEquals(
        "a476985b4cc0d457f247797611799a6f6a80fc8cb7ec9dcb5a8223888d0618e30de165f3d869c4a0d9107d8a5b612ad7c5e42441907f5b91796f0d7187d64a01",
        actual.getData().get(0).getSignature());
    Assertions.assertEquals(
        "7ea09a34aebb13c9841c71397b1cabfec5ddf950405293dee496cac2f437480a",
        actual.getData().get(0).getPublicKey());
    Assertions.assertEquals("ed25519", actual.getData().get(0).getWitnessAlgorithm());
  }

  @Test
  public void testGetAuthorsByAnchor_withSeveralFieldsMightBeNull() {
    String anchorUrl = "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json";
    String anchorHash = "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn(getRawData2());

    var actual =
        governanceActionService.getAuthorsByAnchor(
            "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json",
            "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81",
            PageRequest.of(0, 10));
    verify(offChainVoteGovActionDataRepository)
        .getRawDataByAnchorUrlAndAnchorHash(anchorUrl, anchorHash);
    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals("The Ancient Kraken", actual.getData().get(0).getName());
    Assertions.assertNull(actual.getData().get(0).getSignature());
    Assertions.assertNull(actual.getData().get(0).getPublicKey());
    Assertions.assertNull(actual.getData().get(0).getWitnessAlgorithm());
  }

  @Test
  public void testGetAuthorsByAnchor_withSeveralFieldsMightBeNull1() {
    String anchorUrl = "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json";
    String anchorHash = "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn(getRawData3());

    var actual =
        governanceActionService.getAuthorsByAnchor(
            "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json",
            "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81",
            PageRequest.of(0, 10));
    verify(offChainVoteGovActionDataRepository)
        .getRawDataByAnchorUrlAndAnchorHash(anchorUrl, anchorHash);
    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals("The Ancient Kraken", actual.getData().get(0).getName());
    Assertions.assertNull(actual.getData().get(0).getSignature());
    Assertions.assertNull(actual.getData().get(0).getPublicKey());
    Assertions.assertEquals("ed25519", actual.getData().get(0).getWitnessAlgorithm());
  }

  @Test
  public void testGetAuthorsByAnchor_withRawDataIsNull() {
    String anchorUrl = "anchorUrl";
    String anchorHash = "anchorHash";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn(null);
    var actual =
        governanceActionService.getAuthorsByAnchor(anchorUrl, anchorHash, PageRequest.of(0, 10));
    Assertions.assertEquals(0, actual.getTotalItems());
  }

  @Test
  public void testGetAuthorsByAnchor_withAuthorFieldIsEmpty() {
    String anchorUrl = "anchorUrl";
    String anchorHash = "anchorHash";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn(getRawDataWithMissingAuthorField());
    var actual =
        governanceActionService.getAuthorsByAnchor(anchorUrl, anchorHash, PageRequest.of(0, 10));
    Assertions.assertEquals(0, actual.getTotalItems());
  }

  @Test
  public void testGetAuthorsByAnchor_throwException() throws JsonProcessingException {
    String anchorUrl = "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json";
    String anchorHash = "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81";
    when(offChainVoteGovActionDataRepository.getRawDataByAnchorUrlAndAnchorHash(
            anyString(), anyString()))
        .thenReturn("abc");
    var actual =
        governanceActionService.getAuthorsByAnchor(anchorUrl, anchorHash, PageRequest.of(0, 10));
    Assertions.assertEquals(0, actual.getTotalItems());
  }

  @Test
  void testGetRangeFilterVoteResponse() {
    String txHash = "4864566ddbf27933f7c85f9ab0c58e345de0e40ce03926f5a83be3da2f928ffe";
    Integer index = 0;
    DRepRangeProjection projection = Mockito.mock(DRepRangeProjection.class);
    when(projection.getMaxActiveVoteStake()).thenReturn(BigInteger.valueOf(100));
    when(projection.getMinActiveVoteStake()).thenReturn(BigInteger.valueOf(1));

    when(latestVotingProcedureRepository.getDRepRangeValuesForVotesFilter(anyString(), any()))
        .thenReturn(projection);
    var actual = governanceActionService.getRangeFilterVoteResponse(txHash, index);
    Assertions.assertNotNull(actual);
    Assertions.assertEquals(BigInteger.valueOf(100), actual.getMaxActiveStake());
    Assertions.assertEquals(BigInteger.ONE, actual.getMinActiveStake());
  }

  @Test
  void testGetVotingOnGovAction_withFilterByAnyType() {

    Pageable pageable = PageRequest.of(0, 6, Sort.by("blockTime").descending());

    VoteFilter filter = VoteFilter.builder().txHash("txHash").index(0).voterType(null).build();
    LatestVotingProcedureProjection projection =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(projection.getVoterHash()).thenReturn("voterHash1");
    when(projection.getVoterType()).thenReturn(VoterType.DREP_KEY_HASH);
    when(projection.getVote()).thenReturn(Vote.YES);
    when(projection.getBlockTime()).thenReturn(Long.valueOf(1));

    LatestVotingProcedureProjection projection1 =
        Mockito.mock(LatestVotingProcedureProjection.class);

    when(projection1.getVoterHash()).thenReturn("voterHash2");
    when(projection1.getVoterType()).thenReturn(VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH);
    when(projection1.getVote()).thenReturn(Vote.NO);
    when(projection1.getBlockTime()).thenReturn(Long.valueOf(10));

    when(latestVotingProcedureRepository.getVoteOnGovActionByTypeIn(
            any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(projection, projection1), pageable, 2));

    DRepInfoProjection dRepInfoProjection = Mockito.mock(DRepInfoProjection.class);
    when(dRepInfoProjection.getDrepHash()).thenReturn("voterHash1");
    when(dRepInfoProjection.getActiveVoteStake()).thenReturn(BigInteger.TEN);

    when(drepInfoRepository.findDRepActiveStakeInHashList(anyList()))
        .thenReturn(List.of(dRepInfoProjection));

    var actual = governanceActionService.getVotingOnGovAction(filter, pageable);

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("voterHash1", actual.getData().get(0).getVoterHash());
    Assertions.assertEquals(Vote.YES, actual.getData().get(0).getVote());
    Assertions.assertEquals("voterHash2", actual.getData().get(1).getVoterHash());
    Assertions.assertEquals(Vote.NO, actual.getData().get(1).getVote());
  }

  @Test
  void testGetVotingOnGovAction_withFilterByDRepType() {

    Pageable pageable = PageRequest.of(0, 6, Sort.by("blockTime").descending());

    VoteFilter filter =
        VoteFilter.builder().txHash("txHash").index(0).voterType(VoterType.DREP_KEY_HASH).build();
    LatestVotingProcedureProjection projection =
        Mockito.mock(LatestVotingProcedureProjection.class);
    when(projection.getVoterHash()).thenReturn("voterHash1");
    when(projection.getVoterType()).thenReturn(VoterType.DREP_KEY_HASH);
    when(projection.getVote()).thenReturn(Vote.YES);
    when(projection.getBlockTime()).thenReturn(Long.valueOf(1));

    LatestVotingProcedureProjection projection1 =
        Mockito.mock(LatestVotingProcedureProjection.class);

    when(projection1.getVoterHash()).thenReturn("voterHash2");
    when(projection1.getVoterType()).thenReturn(VoterType.DREP_KEY_HASH);
    when(projection1.getVote()).thenReturn(Vote.NO);
    when(projection1.getBlockTime()).thenReturn(Long.valueOf(10));

    when(latestVotingProcedureRepository.getVoteOnGovActionByDRepType(
            any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new PageImpl<>(List.of(projection, projection1), pageable, 2));

    var actual = governanceActionService.getVotingOnGovAction(filter, pageable);

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("voterHash1", actual.getData().get(0).getVoterHash());
    Assertions.assertEquals(Vote.YES, actual.getData().get(0).getVote());
    Assertions.assertEquals("voterHash2", actual.getData().get(1).getVoterHash());
    Assertions.assertEquals(Vote.NO, actual.getData().get(1).getVote());
  }

  private String getRawData() {
    return "{\n"
        + "  \"@context\": {\n"
        + "    \"@language\": \"en-us\",\n"
        + "    \"CIP100\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#\",\n"
        + "    \"CIP108\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#\",\n"
        + "    \"hashAlgorithm\": \"CIP100:hashAlgorithm\",\n"
        + "    \"body\": {\n"
        + "      \"@id\": \"CIP108:body\",\n"
        + "      \"@context\": {\n"
        + "        \"references\": {\n"
        + "          \"@id\": \"CIP108:references\",\n"
        + "          \"@container\": \"@set\",\n"
        + "          \"@context\": {\n"
        + "            \"GovernanceMetadata\": \"CIP100:GovernanceMetadataReference\",\n"
        + "            \"Other\": \"CIP100:OtherReference\",\n"
        + "            \"label\": \"CIP100:reference-label\",\n"
        + "            \"uri\": \"CIP100:reference-uri\",\n"
        + "            \"referenceHash\": {\n"
        + "              \"@id\": \"CIP108:referenceHash\",\n"
        + "              \"@context\": {\n"
        + "                \"hashDigest\": \"CIP108:hashDigest\",\n"
        + "                \"hashAlgorithm\": \"CIP100:hashAlgorithm\"\n"
        + "              }\n"
        + "            }\n"
        + "          }\n"
        + "        },\n"
        + "        \"title\": \"CIP108:title\",\n"
        + "        \"abstract\": \"CIP108:abstract\",\n"
        + "        \"motivation\": \"CIP108:motivation\",\n"
        + "        \"rationale\": \"CIP108:rationale\"\n"
        + "      }\n"
        + "    },\n"
        + "    \"authors\": {\n"
        + "      \"@id\": \"CIP100:authors\",\n"
        + "      \"@container\": \"@set\",\n"
        + "      \"@context\": {\n"
        + "        \"name\": \"http://xmlns.com/foaf/0.1/name\",\n"
        + "        \"witness\": {\n"
        + "          \"@id\": \"CIP100:witness\",\n"
        + "          \"@context\": {\n"
        + "            \"witnessAlgorithm\": \"CIP100:witnessAlgorithm\",\n"
        + "            \"publicKey\": \"CIP100:publicKey\",\n"
        + "            \"signature\": \"CIP100:signature\"\n"
        + "          }\n"
        + "        }\n"
        + "      }\n"
        + "    }\n"
        + "  },\n"
        + "  \"hashAlgorithm\": \"blake2b-256\",\n"
        + "  \"body\": {\n"
        + "    \"title\": \"Hardfork to Protocol version 10\",\n"
        + "    \"abstract\": \"Let's have sanchoNet in full governance as soon as possible\",\n"
        + "    \"motivation\": \"PV9 is not as fun as PV10\",\n"
        + "    \"rationale\": \"Let's keep testing stuff\",\n"
        + "    \"references\": [\n"
        + "      {\n"
        + "        \"@type\": \"Other\",\n"
        + "        \"label\": \"Hardfork to PV10\",\n"
        + "        \"uri\": \"\"\n"
        + "      }\n"
        + "    ]\n"
        + "  },\n"
        + "  \"authors\": [\n"
        + "    {\n"
        + "      \"name\": \"Carlos\",\n"
        + "      \"witness\": {\n"
        + "        \"witnessAlgorithm\": \"ed25519\",\n"
        + "        \"publicKey\": \"7ea09a34aebb13c9841c71397b1cabfec5ddf950405293dee496cac2f437480a\",\n"
        + "        \"signature\": \"a476985b4cc0d457f247797611799a6f6a80fc8cb7ec9dcb5a8223888d0618e30de165f3d869c4a0d9107d8a5b612ad7c5e42441907f5b91796f0d7187d64a01\"\n"
        + "      }\n"
        + "    }\n"
        + "  ]\n"
        + "}";
  }

  private String getRawData1() {
    return "{\n"
        + "  \"@context\": {\n"
        + "    \"@language\": \"en-us\",\n"
        + "    \"CIP100\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#\",\n"
        + "    \"CIP108\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#\",\n"
        + "    \"hashAlgorithm\": \"CIP100:hashAlgorithm\",\n"
        + "    \"body\": {\n"
        + "      \"@id\": \"CIP108:body\",\n"
        + "      \"@context\": {\n"
        + "        \"references\": {\n"
        + "          \"@id\": \"CIP108:references\",\n"
        + "          \"@container\": \"@set\",\n"
        + "          \"@context\": {\n"
        + "            \"GovernanceMetadata\": \"CIP100:GovernanceMetadataReference\",\n"
        + "            \"Other\": \"CIP100:OtherReference\",\n"
        + "            \"label\": \"CIP100:reference-label\",\n"
        + "            \"uri\": \"CIP100:reference-uri\",\n"
        + "            \"referenceHash\": {\n"
        + "              \"@id\": \"CIP108:referenceHash\",\n"
        + "              \"@context\": {\n"
        + "                \"hashDigest\": \"CIP108:hashDigest\",\n"
        + "                \"hashAlgorithm\": \"CIP100:hashAlgorithm\"\n"
        + "              }\n"
        + "            }\n"
        + "          }\n"
        + "        },\n"
        + "        \"title\": \"CIP108:title\",\n"
        + "        \"abstract\": \"CIP108:abstract\",\n"
        + "        \"motivation\": \"CIP108:motivation\",\n"
        + "        \"rationale\": \"CIP108:rationale\"\n"
        + "      }\n"
        + "    },\n"
        + "    \"authors\": {\n"
        + "      \"@id\": \"CIP100:authors\",\n"
        + "      \"@container\": \"@set\",\n"
        + "      \"@context\": {\n"
        + "        \"name\": \"http://xmlns.com/foaf/0.1/name\",\n"
        + "        \"witness\": {\n"
        + "          \"@id\": \"CIP100:witness\",\n"
        + "          \"@context\": {\n"
        + "            \"witnessAlgorithm\": \"CIP100:witnessAlgorithm\",\n"
        + "            \"publicKey\": \"CIP100:publicKey\",\n"
        + "            \"signature\": \"CIP100:signature\"\n"
        + "          }\n"
        + "        }\n"
        + "      }\n"
        + "    }\n"
        + "  },\n"
        + "  \"hashAlgorithm\": \"blake2b-256\",\n"
        + "  \"body\": {\n"
        + "    \"title\": \"Buy Ryan a island\",\n"
        + "    \"abstract\": \"Withdraw 200000000000 ADA from the treasury so Ryan can buy an island.\",\n"
        + "    \"motivation\": \"The current problem is that Ryan does not have an island, but he would really like an island.\",\n"
        + "    \"rationale\": \"With these funds from the treasury will be sold for **cold hard cash**, this cash can then be used to purchase an island for Ryan. An example of this island is provided in the references.\",\n"
        + "    \"references\": [\n"
        + "      {\n"
        + "        \"@type\": \"Other\",\n"
        + "        \"label\": \"A cool island for Ryan\",\n"
        + "        \"uri\": \"https://www.google.com/maps/place/World's+only+5th+order+recursive+island/@62.6511465,-97.7946829,15.75z/data=!4m14!1m7!3m6!1s0x5216a167810cee39:0x11431abdfe4c7421!2sWorld's+only+5th+order+recursive+island!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n!3m5!1s0x5216a167810cee39:0x11431abdfe4c7421!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n?authuser=0&entry=ttu\"\n"
        + "      }\n"
        + "    ]\n"
        + "  },\n"
        + "  \"authors\": [\n"
        + "    {\n"
        + "      \"name\": \"Ryan Williams\",\n"
        + "      \"witness\": {\n"
        + "        \"witnessAlgorithm\": \"ed25519\",\n"
        + "        \"publicKey\": \"7ea09a34aebb13c9841c71397b1cabfec5ddf950405293dee496cac2f437480a\",\n"
        + "        \"signature\": \"a476985b4cc0d457f247797611799a6f6a80fc8cb7ec9dcb5a8223888d0618e30de165f3d869c4a0d9107d8a5b612ad7c5e42441907f5b91796f0d7187d64a01\"\n"
        + "      }\n"
        + "    }\n"
        + "  ]\n"
        + "}\n";
  }

  private String getRawDataWithMissingAuthorField() {
    return "{\n"
        + "  \"@context\": {\n"
        + "    \"@language\": \"en-us\",\n"
        + "    \"CIP100\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#\",\n"
        + "    \"CIP108\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#\",\n"
        + "    \"hashAlgorithm\": \"CIP100:hashAlgorithm\",\n"
        + "    \"body\": {\n"
        + "      \"@id\": \"CIP108:body\",\n"
        + "      \"@context\": {\n"
        + "        \"references\": {\n"
        + "          \"@id\": \"CIP108:references\",\n"
        + "          \"@container\": \"@set\",\n"
        + "          \"@context\": {\n"
        + "            \"GovernanceMetadata\": \"CIP100:GovernanceMetadataReference\",\n"
        + "            \"Other\": \"CIP100:OtherReference\",\n"
        + "            \"label\": \"CIP100:reference-label\",\n"
        + "            \"uri\": \"CIP100:reference-uri\",\n"
        + "            \"referenceHash\": {\n"
        + "              \"@id\": \"CIP108:referenceHash\",\n"
        + "              \"@context\": {\n"
        + "                \"hashDigest\": \"CIP108:hashDigest\",\n"
        + "                \"hashAlgorithm\": \"CIP100:hashAlgorithm\"\n"
        + "              }\n"
        + "            }\n"
        + "          }\n"
        + "        },\n"
        + "        \"title\": \"CIP108:title\",\n"
        + "        \"abstract\": \"CIP108:abstract\",\n"
        + "        \"motivation\": \"CIP108:motivation\",\n"
        + "        \"rationale\": \"CIP108:rationale\"\n"
        + "      }\n"
        + "    },\n"
        + "    \"authors\": {\n"
        + "      \"@id\": \"CIP100:authors\",\n"
        + "      \"@container\": \"@set\",\n"
        + "      \"@context\": {\n"
        + "        \"name\": \"http://xmlns.com/foaf/0.1/name\",\n"
        + "        \"witness\": {\n"
        + "          \"@id\": \"CIP100:witness\",\n"
        + "          \"@context\": {\n"
        + "            \"witnessAlgorithm\": \"CIP100:witnessAlgorithm\",\n"
        + "            \"publicKey\": \"CIP100:publicKey\",\n"
        + "            \"signature\": \"CIP100:signature\"\n"
        + "          }\n"
        + "        }\n"
        + "      }\n"
        + "    }\n"
        + "  },\n"
        + "  \"authors\": [],\n"
        + "  \"hashAlgorithm\": {\n"
        + "    \"@value\": \"blake2b-256\"\n"
        + "  },\n"
        + "  \"body\": {\n"
        + "    \"abstract\": {\n"
        + "      \"@value\": \"an island or a sports car?\"\n"
        + "    },\n"
        + "    \"motivation\": {\n"
        + "      \"@value\": \"an island or a sports car?\"\n"
        + "    },\n"
        + "    \"rationale\": {\n"
        + "      \"@value\": \"an island or a sports car?\"\n"
        + "    },\n"
        + "    \"references\": [],\n"
        + "    \"title\": {\n"
        + "      \"@value\": \"Info for what should ryan buy with treasury\"\n"
        + "    }\n"
        + "  }\n"
        + "}";
  }

  private String getRawData2() {
    return "{\n"
        + "  \"@context\": {\n"
        + "    \"@language\": \"en-us\",\n"
        + "    \"CIP100\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#\",\n"
        + "    \"CIP108\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#\",\n"
        + "    \"CIP119\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#\",\n"
        + "    \"hashAlgorithm\": \"CIP100:hashAlgorithm\",\n"
        + "    \"body\": {\n"
        + "      \"@id\": \"CIP119:body\",\n"
        + "      \"@context\": {\n"
        + "        \"references\": {\n"
        + "          \"@id\": \"CIP119:references\",\n"
        + "          \"@container\": \"@set\",\n"
        + "          \"@context\": {\n"
        + "            \"GovernanceMetadata\": \"CIP100:GovernanceMetadataReference\",\n"
        + "            \"Other\": \"CIP100:OtherReference\",\n"
        + "            \"label\": \"CIP100:reference-label\",\n"
        + "            \"uri\": \"CIP100:reference-uri\",\n"
        + "            \"referenceHash\": {\n"
        + "              \"@id\": \"CIP108:referenceHash\",\n"
        + "              \"@context\": {\n"
        + "                \"hashDigest\": \"CIP108:hashDigest\",\n"
        + "                \"hashAlgorithm\": \"CIP100:hashAlgorithm\"\n"
        + "              }\n"
        + "            }\n"
        + "          }\n"
        + "        },\n"
        + "        \"comment\": \"CIP100:comment\",\n"
        + "        \"externalUpdates\": {\n"
        + "          \"@id\": \"CIP100:externalUpdates\",\n"
        + "          \"@context\": {\n"
        + "            \"title\": \"CIP100:update-title\",\n"
        + "            \"uri\": \"CIP100:update-uri\"\n"
        + "          }\n"
        + "        },\n"
        + "        \"paymentAddress\": \"CIP119:paymentAddress\",\n"
        + "        \"givenName\": \"CIP119:givenName\",\n"
        + "        \"image\": {\n"
        + "          \"@id\": \"CIP119:image\",\n"
        + "          \"@context\": {\n"
        + "            \"ImageObject\": \"https://schema.org/ImageObject\"\n"
        + "          }\n"
        + "        },\n"
        + "        \"objectives\": \"CIP119:objectives\",\n"
        + "        \"motivations\": \"CIP119:motivations\",\n"
        + "        \"qualifications\": \"CIP119:qualifications\",\n"
        + "        \"title\": \"CIP108:title\",\n"
        + "        \"abstract\": \"CIP108:abstract\",\n"
        + "        \"rationale\": \"CIP108:rationale\"\n"
        + "      }\n"
        + "    },\n"
        + "    \"authors\": {\n"
        + "      \"@id\": \"CIP100:authors\",\n"
        + "      \"@container\": \"@set\",\n"
        + "      \"@context\": {\n"
        + "        \"name\": \"http://xmlns.com/foaf/0.1/name\",\n"
        + "        \"witness\": {\n"
        + "          \"@id\": \"CIP100:witness\",\n"
        + "          \"@context\": {\n"
        + "            \"witnessAlgorithm\": \"CIP100:witnessAlgorithm\",\n"
        + "            \"publicKey\": \"CIP100:publicKey\",\n"
        + "            \"signature\": \"CIP100:signature\"\n"
        + "          }\n"
        + "        }\n"
        + "      }\n"
        + "    }\n"
        + "  },\n"
        + "  \"hashAlgorithm\": \"blake2b-256\",\n"
        + "  \"body\": {\n"
        + "    \"title\": \"A Simple Info Action\",\n"
        + "    \"abstract\": \"This should do nothing. The drep will vote on it.\",\n"
        + "    \"motivations\": \"We need to test things\",\n"
        + "    \"rationale\": \"Without this how can we test things?\",\n"
        + "    \"paymentAddress\": \"addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp\",\n"
        + "    \"givenName\": \"Logical Mechanism Info Action\"\n"
        + "  },\n"
        + "  \"authors\": [\n"
        + "    {\n"
        + "      \"name\": \"The Ancient Kraken\"\n"
        + "    }\n"
        + "  ]\n"
        + "}";
  }

  private String getRawData3() {
    return "{\n"
        + "  \"@context\": {\n"
        + "    \"@language\": \"en-us\",\n"
        + "    \"CIP100\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#\",\n"
        + "    \"CIP108\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#\",\n"
        + "    \"CIP119\": \"https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#\",\n"
        + "    \"hashAlgorithm\": \"CIP100:hashAlgorithm\",\n"
        + "    \"body\": {\n"
        + "      \"@id\": \"CIP119:body\",\n"
        + "      \"@context\": {\n"
        + "        \"references\": {\n"
        + "          \"@id\": \"CIP119:references\",\n"
        + "          \"@container\": \"@set\",\n"
        + "          \"@context\": {\n"
        + "            \"GovernanceMetadata\": \"CIP100:GovernanceMetadataReference\",\n"
        + "            \"Other\": \"CIP100:OtherReference\",\n"
        + "            \"label\": \"CIP100:reference-label\",\n"
        + "            \"uri\": \"CIP100:reference-uri\",\n"
        + "            \"referenceHash\": {\n"
        + "              \"@id\": \"CIP108:referenceHash\",\n"
        + "              \"@context\": {\n"
        + "                \"hashDigest\": \"CIP108:hashDigest\",\n"
        + "                \"hashAlgorithm\": \"CIP100:hashAlgorithm\"\n"
        + "              }\n"
        + "            }\n"
        + "          }\n"
        + "        },\n"
        + "        \"comment\": \"CIP100:comment\",\n"
        + "        \"externalUpdates\": {\n"
        + "          \"@id\": \"CIP100:externalUpdates\",\n"
        + "          \"@context\": {\n"
        + "            \"title\": \"CIP100:update-title\",\n"
        + "            \"uri\": \"CIP100:update-uri\"\n"
        + "          }\n"
        + "        },\n"
        + "        \"paymentAddress\": \"CIP119:paymentAddress\",\n"
        + "        \"givenName\": \"CIP119:givenName\",\n"
        + "        \"image\": {\n"
        + "          \"@id\": \"CIP119:image\",\n"
        + "          \"@context\": {\n"
        + "            \"ImageObject\": \"https://schema.org/ImageObject\"\n"
        + "          }\n"
        + "        },\n"
        + "        \"objectives\": \"CIP119:objectives\",\n"
        + "        \"motivations\": \"CIP119:motivations\",\n"
        + "        \"qualifications\": \"CIP119:qualifications\",\n"
        + "        \"title\": \"CIP108:title\",\n"
        + "        \"abstract\": \"CIP108:abstract\",\n"
        + "        \"rationale\": \"CIP108:rationale\"\n"
        + "      }\n"
        + "    },\n"
        + "    \"authors\": {\n"
        + "      \"@id\": \"CIP100:authors\",\n"
        + "      \"@container\": \"@set\",\n"
        + "      \"@context\": {\n"
        + "        \"name\": \"http://xmlns.com/foaf/0.1/name\",\n"
        + "        \"witness\": {\n"
        + "          \"@id\": \"CIP100:witness\",\n"
        + "          \"@context\": {\n"
        + "            \"witnessAlgorithm\": \"CIP100:witnessAlgorithm\",\n"
        + "            \"publicKey\": \"CIP100:publicKey\",\n"
        + "            \"signature\": \"CIP100:signature\"\n"
        + "          }\n"
        + "        }\n"
        + "      }\n"
        + "    }\n"
        + "  },\n"
        + "  \"hashAlgorithm\": \"blake2b-256\",\n"
        + "  \"body\": {\n"
        + "    \"title\": \"A Simple Info Action\",\n"
        + "    \"abstract\": \"This should do nothing. The drep will vote on it.\",\n"
        + "    \"motivations\": \"We need to test things\",\n"
        + "    \"rationale\": \"Without this how can we test things?\",\n"
        + "    \"paymentAddress\": \"addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp\",\n"
        + "    \"givenName\": \"Logical Mechanism Info Action\"\n"
        + "  },\n"
        + "  \"authors\": [\n"
        + "    {\n"
        + "      \"name\": \"The Ancient Kraken\",\n"
        + "      \"witness\": {\n"
        + "        \"witnessAlgorithm\": \"ed25519\"\n"
        + "      }\n"
        + "    }\n"
        + "  ]\n"
        + "}";
  }
}
