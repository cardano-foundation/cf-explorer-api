package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.VotingProcedureId;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.ledgersync.VotingProcedure;

public interface VotingProcedureRepository
    extends JpaRepository<VotingProcedure, VotingProcedureId> {

  @Query(
      value =
          "select gap.txHash as govActionTxHash, gap.index as govActionIndex, gap.type as govActionType, vp.voterHash as voterHash,"
              + " vp.txHash as votingProcedureTxHash, vp.index as votingProcedureTxIndex, vp.blockTime as blockTime,vp.vote as vote"
              + " from VotingProcedure vp "
              + " join GovActionProposal gap on gap.txHash = vp.govActionTxHash and gap.index = vp.govActionIndex"
              + " where vp.voterHash = :voterHash and (:govActionType is null or gap.type = :govActionType)"
              + " and gap.blockTime >= :blockTime")
  List<VotingProcedureProjection> findVotingProcedureByVoterHashAndGovActionType(
      @Param("voterHash") String voterHash,
      @Param("govActionType") GovActionType govActionType,
      @Param("blockTime") Long blockTime);

  @Query(
      value =
          "select vp.govActionTxHash as govActionTxHash, vp.govActionIndex as govActionIndex, vp.vote as vote, vp.txHash as votingProcedureTxHash, vp.index as votingProcedureTxIndex,"
              + " vp.blockTime as blockTime"
              + " from VotingProcedure vp where vp.govActionTxHash = :txHash and vp.govActionIndex = :index and vp.voterHash = :voterHash and vp.voterType in :voterType"
              + " order by vp.blockTime desc")
  List<VotingProcedureProjection> getVotingProcedureByTxHashAndIndexAndVoterHash(
      @Param("txHash") String txHash,
      @Param("index") Integer index,
      @Param("voterHash") String voterHash,
      @Param("voterType") List<VoterType> voterType);
}
