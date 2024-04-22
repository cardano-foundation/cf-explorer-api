package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.GovActionProposalId;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.GovActionProposal;

@Repository
public interface GovernanceActionRepository
    extends JpaRepository<GovActionProposal, GovActionProposalId> {
  @Query(
      value =
          "select gap.txHash as txHash, gap.index as index, vp.vote as vote, vp.slot as slot, "
              + "gap.type as type, vp.repeatVote as repeatVote, "
              + "gapInfo.status as status, gapInfo.votingPower as votingPower "
              + "from GovActionProposal gap "
              + " join GovActionProposalInfo gapInfo on (gap.txHash = gapInfo.txHash and gap.index = gapInfo.index)"
              + " left join LatestVotingProcedure vp on ("
              + " vp.govActionTxHash = gap.txHash"
              + " and vp.govActionIndex = gap.index"
              + " and vp.voterHash = :voterHash)"
              + " where (:vote is null or (vp.vote = :vote) or (:isVoteNone = true and vp is null))"
              + " and (:isRepeatVote is null or (vp.repeatVote = :isRepeatVote))"
              + " and (:gapStatus is null or (gapInfo.status = :gapStatus))"
              + " and gap.type in (:type)"
              + " and (gap.slot >= :slot)"
              + " and (gap.blockTime >= :from)"
              + " and (gap.blockTime <= :to)"
              + " and (:txHash is null or gap.txHash = :txHash)"
              + " and (:anchorText is null or gap.anchorUrl like %:anchorText% or gap.anchorHash like %:anchorText%)")
  Page<GovernanceActionProjection> getAllByFilter(
      @Param("isRepeatVote") Boolean isRepeatVote,
      @Param("gapStatus") GovActionStatus gapStatus,
      @Param("vote") Vote vote,
      @Param("voterHash") String dRepHash,
      @Param("type") List<GovActionType> type,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("slot") Long slot,
      @Param("txHash") String txHash,
      @Param("anchorText") String anchorText,
      @Param("isVoteNone") Boolean isVoteNone,
      Pageable pageable);

  @Query(
      value =
          "select gap.txHash as txHash, gap.index as index, gap.anchorHash as anchorHash, gap.anchorUrl as anchorUrl,"
              + " gap.type as type, gap.details as details, gap.epoch as epoch, gap.blockTime as blockTime, gapi.status as status, gapi.votingPower as votingPower"
              + " from GovActionProposal gap"
              + " join GovActionProposalInfo gapi on gapi.txHash = gap.txHash and gapi.index = gap.index"
              + " where gap.txHash = :txHash and gap.index = :index")
  Optional<GovActionDetailsProjection> getGovActionDetailsByTxHashAndIndex(
      @Param("txHash") String txHash, @Param("index") Integer index);

  @Query(value = "select count(*) from GovActionProposal gap where gap.blockTime >= :blockTime")
  Long countGovActionThatAllowedToVoteByDRep(@Param("blockTime") Long blockTime);
}
