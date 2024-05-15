package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

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
              + " where lvp.voterHash = :dRepHash and lvp.blockTime >= :blockTime and gap.blockTime >= :blockTime")
  Long countVoteByDRepHash(@Param("dRepHash") String dRepHash, @Param("blockTime") Long blockTime);
}
