package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.time.LocalDateTime;
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

import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.mapper.DRepMapper;
import org.cardanofoundation.explorer.api.model.request.drep.DRepFilterRequest;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepStatusCountProjection;
import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationVoteRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.impl.DRepServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.DRepInfo;

@ExtendWith(MockitoExtension.class)
public class DRepServiceTest {
  @Mock DRepRegistrationRepository dRepRegistrationRepository;
  @Mock DrepInfoRepository drepInfoRepository;
  @Mock VotingProcedureRepository votingProcedureRepository;
  @Mock LatestVotingProcedureRepository latestVotingProcedureRepository;
  @Mock GovernanceActionRepository governanceActionRepository;
  @Mock EpochService epochService;
  @Mock DelegationVoteRepository delegationVoteRepository;
  @InjectMocks DRepServiceImpl dRepCertificateService;
  @InjectMocks DRepServiceImpl dRepService;
  @Mock FetchRewardDataService fetchRewardDataService;

  @Spy
  private DRepCertificateMapper dRepCertificateMapper =
      Mappers.getMapper(DRepCertificateMapper.class);

  @Spy private DRepMapper dRepMapper = Mappers.getMapper(DRepMapper.class);

  @Test
  public void testGetDRepCertificateHistory() {
    String drepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";
    DRepCertificateProjection dRepCertificateProjection1 =
        Mockito.mock(DRepCertificateProjection.class);
    when(dRepCertificateProjection1.getTxHash())
        .thenReturn("947e3c4acb552d04ad22259768d96420f43b8d354048ae596f2439fc9c099fca");
    when(dRepCertificateProjection1.getTxIndex()).thenReturn(0L);
    when(dRepCertificateProjection1.getEpochNo()).thenReturn(10);
    when(dRepCertificateProjection1.getAbsoluteSlot()).thenReturn(1000L);
    when(dRepCertificateProjection1.getBlockNo()).thenReturn(100L);
    when(dRepCertificateProjection1.getSlotNo()).thenReturn(50L);
    when(dRepCertificateProjection1.getType()).thenReturn(DRepActionType.REG_DREP_CERT);

    DRepCertificateProjection dRepCertificateProjection2 =
        Mockito.mock(DRepCertificateProjection.class);
    when(dRepCertificateProjection2.getTxHash())
        .thenReturn("947e3c4acb552d04ad22259768d96420f43b8d354048ae596f2439fc9c099fca");
    when(dRepCertificateProjection2.getType()).thenReturn(DRepActionType.UPDATE_DREP_CERT);

    when(dRepRegistrationRepository.getDRepCertificateByDRepIdOrHash(drepHash))
        .thenReturn(List.of(dRepCertificateProjection1, dRepCertificateProjection2));

    var actual =
        dRepCertificateService.getTxDRepCertificateHistory(
            drepHash, PageRequest.of(0, 2, Sort.by("createdAt").descending()));

    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals(
        actual.getData().get(0).getActionTypes(),
        List.of(DRepActionType.REG_DREP_CERT, DRepActionType.UPDATE_DREP_CERT));
  }

  @Test
  public void testGetDRepDetail() {
    String drepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";
    DRepInfo dRepInfo =
        DRepInfo.builder()
            .anchorUrl("local")
            .drepHash(drepHash)
            .drepId("dRepId")
            .liveStake(BigInteger.TEN)
            .delegators(10)
            .activeVoteStake(BigInteger.TWO)
            .createdAt(1000L)
            .build();
    when(drepInfoRepository.findByDRepHashOrDRepId(drepHash)).thenReturn(Optional.of(dRepInfo));
    when(governanceActionRepository.countGovActionThatAllowedToVoteByBlockTimeGreaterThan(
            dRepInfo.getCreatedAt()))
        .thenReturn(0L);

    var actual = dRepCertificateService.getDRepDetails(drepHash);

    Assertions.assertEquals(dRepInfo.getDrepId(), actual.getDrepId());
    Assertions.assertEquals(10, actual.getDelegators());
    Assertions.assertEquals(BigInteger.TEN, actual.getLiveStake());
    Assertions.assertNull(actual.getVotingParticipation());
  }

  @Test
  public void testGetVoteProcedureChart() {
    String drepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";

    VotingProcedureProjection vote1 = Mockito.mock(VotingProcedureProjection.class);

    DRepInfo dRepInfo =
        DRepInfo.builder().drepHash(drepHash).drepId("dRepId").createdAt(0L).build();

    when(drepInfoRepository.findByDRepHashOrDRepId(drepHash)).thenReturn(Optional.of(dRepInfo));

    when(vote1.getGovActionTxHash()).thenReturn("hash");
    when(vote1.getGovActionIndex()).thenReturn(1);
    when(vote1.getBlockTime()).thenReturn(1000L);

    VotingProcedureProjection vote2 = Mockito.mock(VotingProcedureProjection.class);

    when(vote2.getGovActionTxHash()).thenReturn("hash");
    when(vote2.getGovActionIndex()).thenReturn(1);
    when(vote2.getBlockTime()).thenReturn(1001L);
    when(vote2.getVote()).thenReturn(Vote.NO);

    VotingProcedureProjection vote3 = Mockito.mock(VotingProcedureProjection.class);

    when(vote3.getGovActionTxHash()).thenReturn("hash1");
    when(vote3.getGovActionIndex()).thenReturn(0);
    when(vote3.getVote()).thenReturn(Vote.ABSTAIN);

    VotingProcedureProjection vote4 = Mockito.mock(VotingProcedureProjection.class);

    when(vote4.getGovActionTxHash()).thenReturn("hash2");
    when(vote4.getGovActionIndex()).thenReturn(0);
    when(vote4.getVote()).thenReturn(Vote.YES);

    VotingProcedureProjection vote5 = Mockito.mock(VotingProcedureProjection.class);

    when(vote5.getGovActionTxHash()).thenReturn("hash3");
    when(vote5.getGovActionIndex()).thenReturn(1);
    when(vote5.getVote()).thenReturn(Vote.YES);

    when(votingProcedureRepository.findVotingProcedureByVoterHashAndGovActionType(
            drepHash, GovActionType.TREASURY_WITHDRAWALS_ACTION, 0L))
        .thenReturn(List.of(vote1, vote2, vote3, vote4, vote5));

    var actual =
        dRepCertificateService.getVoteProcedureChart(
            drepHash, GovActionType.TREASURY_WITHDRAWALS_ACTION);

    Assertions.assertEquals(2, actual.getNumberOfYesVote());
    Assertions.assertEquals(1, actual.getNumberOfAbstainVotes());
    Assertions.assertEquals(1, actual.getNumberOfNoVotes());
  }

  @Test
  void testGetDRepOverview() {
    EpochSummary epochSummary =
        EpochSummary.builder().no(100).slot(1000).endTime(LocalDateTime.now().minusDays(5)).build();

    DRepStatusCountProjection projection1 = Mockito.mock(DRepStatusCountProjection.class);
    when(projection1.getStatus()).thenReturn(DRepStatus.ACTIVE);
    when(projection1.getCnt()).thenReturn(10L);

    DRepStatusCountProjection projection2 = Mockito.mock(DRepStatusCountProjection.class);
    when(projection2.getStatus()).thenReturn(DRepStatus.INACTIVE);
    when(projection2.getCnt()).thenReturn(5L);

    when(drepInfoRepository.getDRepStatusCount()).thenReturn(List.of(projection1, projection2));
    when(drepInfoRepository.getDelegateCount()).thenReturn(100L);
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);

    var actual = dRepCertificateService.getDRepOverview();

    Assertions.assertEquals(10, actual.getActiveDReps());
    Assertions.assertEquals(5, actual.getInactiveDReps());
    Assertions.assertEquals(100, actual.getDelegators());
    Assertions.assertEquals(100, actual.getEpochNo());
  }

  @Test
  void testGetDRepListByFilter() {
    DRepFilterRequest filter =
        DRepFilterRequest.builder()
            .activeStakeFrom(BigInteger.ONE)
            .activeStakeTo(BigInteger.TEN)
            .votingPowerFrom(0.5)
            .votingPowerTo(1.0)
            .drepStatus(DRepStatus.ACTIVE)
            .build();

    DRepInfo response1 =
        DRepInfo.builder()
            .votingPower(0.6)
            .activeVoteStake(BigInteger.TWO)
            .status(DRepStatus.ACTIVE)
            .build();

    DRepInfo response2 =
        DRepInfo.builder()
            .votingPower(0.7)
            .activeVoteStake(BigInteger.TWO)
            .status(DRepStatus.ACTIVE)
            .build();

    when(drepInfoRepository.getDRepInfoByFilterRequest(
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(Pageable.class)))
        .thenReturn(new PageImpl<>(List.of(response1, response2)));

    var actual = dRepCertificateService.getDRepsByFilter(filter, PageRequest.of(0, 2));

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals(0.6, actual.getData().get(0).getVotingPower());
    Assertions.assertEquals(BigInteger.TWO, actual.getData().get(1).getActiveVoteStake());
  }

  @Test
  public void testDRepGetDelegators() {
    String dRepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";

    Pageable pageable = PageRequest.of(0, 10, Sort.by("createdAt").descending());

    DRepDelegatorProjection dRepDelegatorProjection1 = Mockito.mock(DRepDelegatorProjection.class);
    when(dRepDelegatorProjection1.getStakeAddress()).thenReturn("address1");
    when(dRepDelegatorProjection1.getBlockTime()).thenReturn(10L);

    DRepDelegatorProjection dRepDelegatorProjection2 = Mockito.mock(DRepDelegatorProjection.class);
    when(dRepDelegatorProjection2.getStakeAddress()).thenReturn("address2");
    when(dRepDelegatorProjection2.getBlockTime()).thenReturn(20L);

    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(delegationVoteRepository.getDelegationVoteByDRepHashOrDRepId(anyString(), any()))
        .thenReturn(new PageImpl<>(List.of(dRepDelegatorProjection1, dRepDelegatorProjection2)));
    var actual = dRepService.getDRepDelegators(dRepHash, pageable);

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("address1", actual.getData().get(0).getStakeAddress());
  }

  @Test
  public void testDRepGetDelegators_withoutPageable() {
    String dRepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";

    DRepDelegatorProjection dRepDelegatorProjection1 = Mockito.mock(DRepDelegatorProjection.class);
    when(dRepDelegatorProjection1.getStakeAddress()).thenReturn("address1");
    when(dRepDelegatorProjection1.getBlockTime()).thenReturn(10L);

    DRepDelegatorProjection dRepDelegatorProjection2 = Mockito.mock(DRepDelegatorProjection.class);
    when(dRepDelegatorProjection2.getStakeAddress()).thenReturn("address2");
    when(dRepDelegatorProjection2.getBlockTime()).thenReturn(20L);

    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(delegationVoteRepository.getDelegationVoteByDRepHashOrDRepId(anyString(), any()))
        .thenReturn(new PageImpl<>(List.of(dRepDelegatorProjection1, dRepDelegatorProjection2)));
    var actual = dRepService.getDRepDelegators(dRepHash, PageRequest.of(0, 10));

    Assertions.assertEquals(2, actual.getTotalItems());
    Assertions.assertEquals("address1", actual.getData().get(0).getStakeAddress());
  }
}
