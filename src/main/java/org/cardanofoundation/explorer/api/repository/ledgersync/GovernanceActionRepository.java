package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.GovActionProposal;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.GovActionProposalId;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

@Repository
public interface GovernanceActionRepository
    extends JpaRepository<GovActionProposal, GovActionProposalId> {
  @Query(
      value =
          "select gap.txHash as txHash, gap.index as index, vp.vote as vote, vp.slot as slot, gap.type as type from GovActionProposal gap"
              + " left join VotingProcedure vp on ("
              + " vp.govActionTxHash = gap.txHash"
              + " and vp.govActionIndex = gap.index"
              + " and vp.voterHash = :voterHash"
              + " and not exists (select 1 from VotingProcedure vp2"
              + "                    where vp2.govActionTxHash = gap.txHash"
              + "                    and vp2.govActionIndex = gap.index"
              + "                    and vp2.voterHash = :voterHash"
              + "                    and vp2.blockTime > vp.blockTime))"
              + " where (:type is null or gap.type = :type)"
              + " and (:vote is null or (vp.vote = :vote and vp.slot >= :slotDRep))"
              + " and (:from is null or vp.blockTime >= :from)"
              + " and (:to is null or vp.blockTime <= :to)")
  Page<GovernanceActionProjection> getAllByFilter(
      @Param("vote") Vote vote,
      @Param("voterHash") String dRepHash,
      @Param("type") GovActionType type,
      @Param("from") Timestamp from,
      @Param("to") Timestamp to,
      @Param("slotDRep") Long slotDRep,
      Pageable pageable);
}
