package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.CountVoteOnGovActionProjection;
import org.cardanofoundation.explorer.api.projection.LatestVotingProcedureProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestVotingProcedure;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.LatestVotingProcedureId;

@Repository
public interface LatestVotingProcedureRepository
    extends JpaRepository<LatestVotingProcedure, LatestVotingProcedureId> {

  @Query(
      value =
          "select lvp.voterHash,lvp.vote as vote, lvp.voterType as voterType"
              + " from LatestVotingProcedure lvp"
              + " where lvp.govActionTxHash = :govActionTxHash and lvp.govActionIndex = :govActionIndex")
  List<CountVoteOnGovActionProjection> getLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
      @Param("govActionTxHash") String govActionTxHash,
      @Param("govActionIndex") Integer govActionIndex);

  @Query(
      value =
          " select lvp.vote as vote, lvp.govActionTxHash as govActionTxHash, lvp.govActionIndex as govActionIndex from LatestVotingProcedure lvp where lvp.voterHash = :dRepHash and lvp.blockTime >= :blockTime")
  List<LatestVotingProcedureProjection> findVotingByDRepHash(
      @Param("dRepHash") String dRepHash, @Param("blockTime") Long slot);
}
