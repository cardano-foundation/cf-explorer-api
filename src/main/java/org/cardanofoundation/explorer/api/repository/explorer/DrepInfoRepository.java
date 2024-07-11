package org.cardanofoundation.explorer.api.repository.explorer;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepStatusCountProjection;
import org.cardanofoundation.explorer.api.projection.DRepInfoProjection;
import org.cardanofoundation.explorer.api.projection.DRepRangeProjection;
import org.cardanofoundation.explorer.common.entity.enumeration.DRepStatus;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;

public interface DrepInfoRepository extends JpaRepository<DRepInfo, Long> {

  @Query(
      value =
          " select dri from DRepInfo dri"
              + " where dri.drepHash = :dRepHashOrDRepId or dri.drepId = :dRepHashOrDRepId")
  Optional<DRepInfo> findByDRepHashOrDRepId(@Param("dRepHashOrDRepId") String dRepHashOrDRepId);

  @Query("SELECT dri.status as status, count(dri) as cnt from DRepInfo dri group by dri.status")
  List<DRepStatusCountProjection> getDRepStatusCount();

  @Query("SELECT count(dri) from DRepInfo dri where dri.status = :status")
  Long countByStatus(@Param("status") DRepStatus status);

  @Query("SELECT SUM(dri.delegators) from DRepInfo dri")
  Long getDelegateCount();

  @Query(
      value =
          """
    SELECT dri.drepHash as drepHash, dri.activeVoteStake as activeVoteStake
    FROM DRepInfo dri
    WHERE dri.createdAt <= :blockTime and dri.status != 'RETIRED'
    """)
  List<DRepInfoProjection> findDRepByCreatedAt(@Param("blockTime") Long blockTime);

  @Query(
      value =
          " select dri from DRepInfo dri"
              + " where (:dRepHashOrDRepId is null or (dri.drepHash = :dRepHashOrDRepId or dri.drepId = :dRepHashOrDRepId))"
              + " and (:anchorText is null or (dri.anchorUrl like %:anchorText% or dri.anchorHash like %:anchorText%)) "
              + " and (coalesce(dri.activeVoteStake,0) >= :activeStakeFrom and coalesce(dri.activeVoteStake,0) <= :activeStakeTo)"
              + " and (coalesce(dri.votingPower,0) >= :votingPowerFrom and coalesce(dri.votingPower,0) <= :votingPowerTo)"
              + " and (:dRepStatus is null or dri.status = :dRepStatus)"
              + " and (dri.govParticipationRate >= :minGovParticipationRate and dri.govParticipationRate <= :maxGovParticipationRate)"
              + " and (dri.createdAt >= :fromDate and dri.createdAt <= :toDate)")
  Page<DRepInfo> getDRepInfoByFilterRequest(
      @Param("dRepHashOrDRepId") String dRepHashOrDRepId,
      @Param("anchorText") String anchorText,
      @Param("activeStakeFrom") BigInteger activeStakeFrom,
      @Param("activeStakeTo") BigInteger activeStakeTo,
      @Param("votingPowerFrom") Double votingPowerFrom,
      @Param("votingPowerTo") Double votingPowerTo,
      @Param("dRepStatus") DRepStatus dRepStatus,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate,
      @Param("minGovParticipationRate") Double minGovParticipationRate,
      @Param("maxGovParticipationRate") Double maxGovParticipationRate,
      Pageable pageable);

  @Query(
      value =
          """
      select max(dri.votingPower) as maxVotingPower , max(dri.activeVoteStake) as maxActiveVoteStake,
       min(dri.votingPower) as minVotingPower , min(dri.activeVoteStake) as minActiveVoteStake,
       min(dri.govParticipationRate) as minGovParticipationRate, max(dri.govParticipationRate) as maxGovParticipationRate from DRepInfo dri
          """)
  DRepRangeProjection getDRepRangeValues();
}
