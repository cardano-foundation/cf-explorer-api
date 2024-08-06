package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.DRepRangeProjection;
import org.cardanofoundation.explorer.api.projection.LatestVotingProcedureProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.LatestVotingProcedureId;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestVotingProcedure;

@Repository
public interface LatestVotingProcedureRepository
    extends JpaRepository<LatestVotingProcedure, LatestVotingProcedureId> {

  @Query(
      value =
          """
              SELECT lvp.voterHash as voterHash, lvp.vote as vote, lvp.voterType as voterType
              FROM LatestVotingProcedure lvp
              JOIN CommitteeInfo ci ON lvp.voterHash = ci.hotKey and ci.createdAt <= :blockTime
              WHERE lvp.govActionTxHash = :govActionTxHash and lvp.govActionIndex = :govActionIndex
              and lvp.voterType in :voterType""")
  List<LatestVotingProcedureProjection> getLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
      @Param("govActionTxHash") String govActionTxHash,
      @Param("govActionIndex") Integer govActionIndex,
      @Param("voterType") List<VoterType> voterType,
      @Param("blockTime") Long blockTime);

  @Query(
      value =
          """
  select lvp.voterHash as voterHash, lvp.vote as vote from LatestVotingProcedure lvp
  where lvp.govActionTxHash = :govActionTxHash
  and lvp.govActionIndex = :govActionIndex
  and lvp.voterType in :voterType
  and lvp.voterHash in :voterHashList
  """)
  List<LatestVotingProcedureProjection> findByGovActionTxHashAndGovActionIndex(
      @Param("govActionTxHash") String govActionTxHash,
      @Param("govActionIndex") Integer govActionIndex,
      @Param("voterHashList") List<String> voterHashList,
      @Param("voterType") List<VoterType> voterType);

  @Query(
      value =
          "select count(*) from LatestVotingProcedure lvp"
              + " join GovActionProposal gap on gap.txHash = lvp.govActionTxHash and gap.index = lvp.govActionIndex"
              + " where lvp.voterHash = :voterHash and lvp.blockTime >= :blockTime and gap.blockTime >= :blockTime")
  Long countVoteByVoterHash(
      @Param("voterHash") String voterHash, @Param("blockTime") Long blockTime);

  @Query(
      """
    SELECT lvp.voterHash as voterHash, lvp.vote as vote, lvp.voterType as voterType,
    lvp.blockTime as blockTime, lvp.repeatVote as repeatVote
    FROM LatestVotingProcedure lvp
    WHERE lvp.govActionTxHash = :txHash AND lvp.govActionIndex = :index
    AND (:voterHash is null or lvp.voterHash = :voterHash)
    AND (lvp.blockTime >= :from AND lvp.blockTime <= :to)
    AND lvp.voterType in :voterType""")
  Page<LatestVotingProcedureProjection> getVoteOnGovActionByAnyType(
      @Param("txHash") String txHash,
      @Param("index") Integer index,
      @Param("voterHash") String voterHash,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("voterType") List<VoterType> voterType,
      Pageable pageable);

  @Query(
      """
    SELECT lvp.voterHash as voterHash, lvp.vote as vote, lvp.voterType as voterType,
    lvp.blockTime as blockTime, lvp.repeatVote as repeatVote, di.activeVoteStake AS votingStake
    FROM LatestVotingProcedure lvp
    LEFT JOIN DRepInfo di ON lvp.voterHash = di.drepHash
    WHERE lvp.govActionTxHash = :txHash AND lvp.govActionIndex = :index
    AND (:voterHash is null or lvp.voterHash = :voterHash)
    AND (lvp.blockTime >= :from AND lvp.blockTime <= :to)
    AND lvp.voterType in :voterType
    AND (di.activeVoteStake >= :votingStakeFrom AND di.activeVoteStake <= :votingStakeTo)""")
  Page<LatestVotingProcedureProjection> getVoteOnGovActionByDRepType(
      @Param("txHash") String txHash,
      @Param("index") Integer index,
      @Param("voterHash") String voterHash,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("voterType") List<VoterType> voterType,
      @Param("votingStakeFrom") BigInteger votingStakeFrom,
      @Param("votingStakeTo") BigInteger votingStakeTo,
      Pageable pageable);

  @Query(
      """
    SELECT max(dri.votingPower) as maxVotingPower , max(dri.activeVoteStake) as maxActiveVoteStake,
           min(dri.votingPower) as minVotingPower , min(dri.activeVoteStake) as minActiveVoteStake,
           min(dri.govParticipationRate) as minGovParticipationRate, max(dri.govParticipationRate) as maxGovParticipationRate
    FROM LatestVotingProcedure lvp
    LEFT JOIN DRepInfo dri on lvp.voterHash = dri.drepHash
    WHERE lvp.govActionTxHash = :txHash and lvp.govActionIndex = :index
    """)
  DRepRangeProjection getDRepRangeValuesForVotesFilter(
      @Param("txHash") String txHash, @Param("index") Integer index);
}
