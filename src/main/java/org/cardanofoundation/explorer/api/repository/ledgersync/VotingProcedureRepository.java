package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.VotingProcedure;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.VotingProcedureId;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;

public interface VotingProcedureRepository
    extends JpaRepository<VotingProcedure, VotingProcedureId> {

  @Query(
      value =
          "select gap.txHash as govActionTxHash, gap.index as govActionIndex, gap.type as govActionType, vp.voterHash as voterHash,"
              + " vp.txHash as votingProcedureTxHash, vp.index as votingProcedureTxIndex, vp.blockTime as blockTime,vp.vote as vote"
              + " from VotingProcedure vp "
              + " join GovActionProposal gap on gap.txHash = vp.govActionTxHash and gap.index = vp.govActionIndex"
              + " where vp.voterHash = :voterHash and (:govActionType is null or gap.type = :govActionType)")
  List<VotingProcedureProjection> findVotingProcedureByVoterHashAndGovActionType(
      @Param("voterHash") String voterHash, @Param("govActionType") GovActionType govActionType);
}
