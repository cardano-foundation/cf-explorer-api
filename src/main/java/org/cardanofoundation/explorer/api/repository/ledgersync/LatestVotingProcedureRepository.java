package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.CountVoteOnGovActionProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestVotingProcedure;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.LatestVotingProcedureId;

@Repository
public interface LatestVotingProcedureRepository
    extends JpaRepository<LatestVotingProcedure, LatestVotingProcedureId> {

  @Query(
      value =
          "select count(*), lvp.vote as vote, lvp.voter_type as voterType"
              + " from latest_voting_procedure lvp"
              + " where lvp.gov_action_tx_hash = :govActionTxHash and lvp.gov_action_index = :govActionIndex and lvp.repeat_vote is false"
              + " group by vote, voterType",
      nativeQuery = true)
  List<CountVoteOnGovActionProjection> countLatestVotingProcedureByGovActionTxHashAndGovActionIndex(
      @Param("govActionTxHash") String govActionTxHash,
      @Param("govActionIndex") Integer govActionIndex);

  @Query(
      value =
          " select count(*) from LatestVotingProcedure lvp where lvp.voterHash = :dRepHash and lvp.blockTime >= :blockTime")
  Long countVoteByDRepHash(@Param("dRepHash") String dRepHash, @Param("blockTime") Long blockTime);
}
