package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageRequest;
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
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.GovernanceActionRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.LatestVotingProcedureRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.VotingProcedureRepository;
import org.cardanofoundation.explorer.api.service.impl.DRepServiceImpl;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.DRepActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

@ExtendWith(MockitoExtension.class)
public class DRepServiceTest {
  @Mock DRepRegistrationRepository dRepRegistrationRepository;
  @Mock DrepInfoRepository drepInfoRepository;
  @Mock VotingProcedureRepository votingProcedureRepository;
  @Mock LatestVotingProcedureRepository latestVotingProcedureRepository;
  @Mock GovernanceActionRepository governanceActionRepository;
  @InjectMocks DRepServiceImpl dRepCertificateService;

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
    when(governanceActionRepository.countGovActionThatAllowedToVoteByDRep(dRepInfo.getCreatedAt()))
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

    DRepInfo dRepInfo =
        DRepInfo.builder().drepHash(drepHash).drepId("dRepId").createdAt(0L).build();
    when(drepInfoRepository.findByDRepHashOrDRepId(drepHash)).thenReturn(Optional.of(dRepInfo));

    VotingProcedureProjection vote1 = Mockito.mock(VotingProcedureProjection.class);

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
            drepHash,
            org.cardanofoundation.explorer.api.common.enumeration.GovActionType
                .TREASURY_WITHDRAWALS_ACTION);

    Assertions.assertEquals(2, actual.getNumberOfYesVote());
    Assertions.assertEquals(1, actual.getNumberOfAbstainVotes());
    Assertions.assertEquals(1, actual.getNumberOfNoVotes());
  }
}
