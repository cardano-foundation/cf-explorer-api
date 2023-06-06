package org.cardanofoundation.explorer.api.repository;

import java.util.List;
import java.util.Set;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiOsProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolHistoryRepository extends JpaRepository<PoolHistory, Long> {

  @Query(value =
      "SELECT ph.poolId AS view, CAST(ph.delegRewards AS BigInteger) AS delegateReward, ph.epochRos AS ros "
          + "FROM PoolHistory ph "
          + "WHERE ph.poolId IN :poolIds AND ph.epochNo = :epochNo")
  List<PoolHistoryKoiOsProjection> getPoolHistoryKoiOs(@Param("poolIds") Set<String> poolIds,
      @Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT ph.epochNo AS epochNo, CAST(ph.delegRewards AS BigInteger) AS delegateReward, ph.epochRos AS ros, "
          + "CAST(ph.activeStake AS BigInteger) AS activeStake, CAST(ph.poolFees AS BigInteger) AS poolFees "
          + "FROM PoolHistory ph "
          + "WHERE ph.poolId = :poolId "
          + "ORDER BY ph.epochNo DESC")
  Page<PoolHistoryKoiOsProjection> getPoolHistoryKoiOs(@Param("poolId") String poolId, Pageable pageable);

  @Query(value =
          "SELECT ph.epochNo AS epochNo, CAST(ph.delegRewards AS BigInteger) AS delegateReward, ph.epochRos AS ros, "
                  + "CAST(ph.activeStake AS BigInteger) AS activeStake, CAST(ph.poolFees AS BigInteger) AS poolFees "
                  + "FROM PoolHistory ph "
                  + "WHERE ph.poolId = :poolId "
                  + "ORDER BY ph.epochNo DESC")
  List<PoolHistoryKoiOsProjection> getPoolHistoryKoiOs(@Param("poolId") String poolId);

  @Query(value =
      "SELECT ph.epochNo AS chartKey, CAST(ph.activeStake AS BigInteger) AS chartValue "
          + "FROM PoolHistory ph "
          + "WHERE ph.poolId = :poolId "
          + "ORDER BY ph.epochNo ASC")
  List<EpochChartProjection> getPoolHistoryKoiOsForEpochChart(@Param("poolId") String poolId);
}
