package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Set;

import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;
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
      "SELECT ph.pool.view AS view, ph.delegatorRewards AS delegateReward, ph.epochRos AS ros "
          + "FROM PoolHistory ph "
          + "WHERE ph.pool.view IN :poolIds AND ph.epochNo = :epochNo")
  List<PoolHistoryKoiosProjection> getPoolHistoryKoios(@Param("poolIds") Set<String> poolIds,
      @Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT ph.epochNo AS epochNo, ph.delegatorRewards AS delegateReward, ph.epochRos AS ros, "
          + "ph.activeStake AS activeStake, ph.poolFees AS poolFees "
          + "FROM PoolHistory ph "
          + "WHERE ph.pool.view = :poolId "
          + "ORDER BY ph.epochNo DESC")
  Page<PoolHistoryKoiosProjection> getPoolHistoryKoios(@Param("poolId") String poolId, Pageable pageable);

  @Query(value =
      "SELECT ph.epochNo AS epochNo, ph.delegatorRewards AS delegateReward, ph.epochRos AS ros, "
          + "ph.activeStake AS activeStake, ph.poolFees AS poolFees "
          + "FROM PoolHistory ph "
          + "WHERE ph.pool.view = :poolId "
          + "ORDER BY ph.epochNo DESC")
  List<PoolHistoryKoiosProjection> getPoolHistoryKoios(@Param("poolId") String poolId);

  @Query(value =
      "SELECT ph.epochNo AS chartKey, ph.activeStake AS chartValue "
          + "FROM PoolHistory ph "
          + "WHERE ph.pool.view = :poolId "
          + "ORDER BY ph.epochNo ASC")
  List<EpochChartProjection> getPoolHistoryKoiosForEpochChart(@Param("poolId") String poolId);

  @Query(value =
      "SELECT ph.epochNo AS chartKey, CAST(ph.delegatorCnt as long) AS chartValue "
          + "FROM PoolHistory ph "
          + "WHERE ph.pool.view = :poolId "
          + "ORDER BY ph.epochNo ASC")
  List<DelegatorChartProjection> getDataForDelegatorChart(@Param("poolId") String poolId);
}
