package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

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
import org.cardanofoundation.explorer.api.mapper.VotingProcedureMapper;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.projection.DRepInfoProjection;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.api.projection.LatestVotingProcedureProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeMemberRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.CommitteeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.impl.GovernanceActionServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
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

  @Spy
  private GovernanceActionMapper governanceActionMapper =
      Mappers.getMapper(GovernanceActionMapper.class);

  @Spy
  private VotingProcedureMapper votingProcedureMapper =
      Mappers.getMapper(VotingProcedureMapper.class);

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
            .ccThreshold(0.66)
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
}
