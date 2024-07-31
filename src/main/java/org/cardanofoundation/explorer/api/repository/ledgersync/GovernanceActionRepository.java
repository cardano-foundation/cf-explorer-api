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
          """
    SELECT gap.txHash as txHash, gap.index as index, vp.vote as vote, vp.slot as slot, vp.voterHash as voterHash,
              gap.type as type, gap.blockTime as createdAt, vp.repeatVote as repeatVote,
              gapInfo.status as status, gapInfo.votingPower as votingPower, gapInfo.indexType as indexType
              FROM GovActionProposal gap
              LEFT JOIN GovActionProposalInfo gapInfo ON (gap.txHash = gapInfo.txHash and gap.index = gapInfo.index)
              LEFT JOIN LatestVotingProcedure vp ON (
              vp.govActionTxHash = gap.txHash
              AND vp.govActionIndex = gap.index
              AND vp.voterHash in :voterHashes)
              WHERE (:vote is null or (vp.vote = :vote) or (:isVoteNone = true and vp is null))
              AND (:isRepeatVote is null or (vp.repeatVote = :isRepeatVote))
              AND (:gapStatus is null or (gapInfo.status = :gapStatus))
              AND gap.type in (:type)
              AND (gap.slot >= :slot)
              AND (gap.blockTime >= :from)
              AND (gap.blockTime <= :to)
              AND (:txHash is null or gap.txHash = :txHash)
              AND (:anchorText is null or gap.anchorUrl like %:anchorText% or gap.anchorHash like %:anchorText%)
  """)
  Page<GovernanceActionProjection> getAllByFilter(
      @Param("isRepeatVote") Boolean isRepeatVote,
      @Param("gapStatus") GovActionStatus gapStatus,
      @Param("vote") Vote vote,
      @Param("voterHashes") List<String> voterHashes,
      @Param("type") List<GovActionType> type,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("slot") Long slot,
      @Param("txHash") String txHash,
      @Param("anchorText") String anchorText,
      @Param("isVoteNone") Boolean isVoteNone,
      Pageable pageable);

  @Query(
      """
      SELECT gap.txHash          AS txHash,
             gap.index           AS index,
             gap.type            AS type,
             gap.blockTime       AS createdAt,
             gapInfo.status      AS status,
             gapInfo.votingPower AS votingPower,
             gapInfo.indexType   AS indexType,
             offChain.title      AS govActionName
      FROM GovActionProposal gap
          LEFT JOIN GovActionProposalInfo gapInfo ON (gap.txHash = gapInfo.txHash and gap.index = gapInfo.index)
          LEFT JOIN OffChainVoteGovActionData offChain ON offChain.anchorUrl = gap.anchorUrl
      WHERE (:gapStatus is null or (gapInfo.status = :gapStatus))
          AND gap.type in (:type)
          AND (gap.blockTime >= :from)
          AND (gap.blockTime <= :to)
          AND (:txHash is null or gap.txHash = :txHash)
          AND (:anchorText is null or gap.anchorUrl like %:anchorText% or gap.anchorHash like %:anchorText%)
      """)
  Page<GovernanceActionProjection> getAllByFilter(
      @Param("gapStatus") GovActionStatus gapStatus,
      @Param("type") List<GovActionType> type,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("txHash") String txHash,
      @Param("anchorText") String anchorText,
      Pageable pageable);

  @Query(
      """
      SELECT gap.txHash as txHash, gap.index as index, gap.blockTime as createdAt,
      gapInfo.status as status, gap.type as type
      FROM GovActionProposal gap
      JOIN GovActionProposalInfo gapInfo ON (gap.txHash = gapInfo.txHash and gap.index = gapInfo.index)
      WHERE gap.type in :type
      AND gapInfo.status = 'ENACTED'
      AND (gap.blockTime >= :from)
      AND (gap.blockTime <= :to)
      AND (:txHash is null or gap.txHash = :txHash)
      AND (:anchorText is null or gap.anchorUrl like %:anchorText% or gap.anchorHash like %:anchorText%)
  """)
  Page<GovernanceActionProjection> getAllGovCommitteeHistory(
      @Param("type") List<GovActionType> type,
      @Param("from") Long from,
      @Param("to") Long to,
      @Param("txHash") String txHash,
      @Param("anchorText") String anchorText,
      Pageable pageable);

  @Query(
      value =
          """
    SELECT gap.txHash as txHash, gap.index as index, gap.anchorHash as anchorHash, gap.anchorUrl as anchorUrl,
    gap.type as type, gap.details as details, gap.epoch as epoch, gap.blockTime as blockTime,
    gap.slot as slot, gapi.status as status, gapi.votingPower as votingPower, gapi.indexType as indexType
    FROM GovActionProposal gap
    JOIN GovActionProposalInfo gapi ON gapi.txHash = gap.txHash AND gapi.index = gap.index
    WHERE gap.txHash = :txHash AND gap.index = :index
""")
  Optional<GovActionDetailsProjection> getGovActionDetailsByTxHashAndIndex(
      @Param("txHash") String txHash, @Param("index") Integer index);

  @Query(value = "select count(*) from GovActionProposal gap where gap.blockTime >= :blockTime")
  Long countGovActionThatAllowedToVoteByBlockTimeGreaterThan(@Param("blockTime") Long blockTime);

  @Query(
      value =
          "select count(*) from GovActionProposal gap where gap.blockTime >= :blockTime "
              + "and gap.type in :govTypes ")
  Long countGovThatAllowedToVoteByBlockTimeGreaterThanAndGovType(
      @Param("blockTime") Long blockTime, @Param("govTypes") List<GovActionType> govTypes);

  @Query(
      """
        SELECT gap.type AS type, COUNT(gap) AS govCount
        FROM GovActionProposal gap
        GROUP BY gap.type
        """)
  List<GovernanceActionProjection> getGovActionGroupByGovActionType();

  @Query(
      """
        SELECT gapi.status AS status, COUNT(gapi) AS govCount
        FROM GovActionProposalInfo gapi
        GROUP BY gapi.status
        """)
  List<GovernanceActionProjection> getGovActionGroupByGovActionStatus();
}
