package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.DelegatorChartProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailDelegatorProjection;
import com.cardano.explorer.projection.PoolDelegationSummaryProjection;
import com.sotatek.cardano.common.entity.Delegation;
import com.sotatek.cardano.common.entity.Delegation_;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface DelegationRepository extends JpaRepository<Delegation, Long> {

  @Query("SELECT count(de.id) FROM Delegation de WHERE de.poolHash.id = :poolId")
  Integer numberDelegatorsByPool(@Param("poolId") Long poolId);

  @EntityGraph(attributePaths = {Delegation_.POOL_HASH, Delegation_.ADDRESS})
  List<Delegation> findByTx(Tx tx);

  @Query(value =
      "SELECT dg.activeEpochNo AS chartKey, count(dg.id) AS chartValue FROM Delegation dg "
          + "WHERE dg.poolHash.id = :poolId "
          + "GROUP BY dg.activeEpochNo "
          + "ORDER BY dg.activeEpochNo ASC")
  List<DelegatorChartProjection> getDataForDelegatorChart(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT dg.id AS id, sa.hashRaw AS address, sa.id AS stakeAddressId, bk.time AS time, tx.fee AS fee "
          + "FROM PoolHash ph "
          + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
          + "LEFT JOIN StakeAddress sa ON sa.id = dg.address.id "
          + "JOIN Tx tx ON tx.id = dg.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "WHERE ph.id = :poolId "
          + "ORDER BY dg.id ASC")
  Page<PoolDetailDelegatorProjection> getAllDelegatorByPool(@Param("poolId") Long poolId,
      Pageable pageable);

  @Query(value = "SELECT bk.time AS time, tx.fee AS fee FROM Delegation dg "
      + "JOIN Tx tx ON tx.id = dg.tx.id "
      + "JOIN Block bk ON bk.id = tx.block.id "
      + "WHERE dg.id = :delegatorId")
  PoolDetailDelegatorProjection getTimeAndFeeByDelegator(@Param("delegatorId") Long delegatorId);

  @Query("SELECT count(de.id) FROM Delegation de WHERE de.activeEpochNo = :epochNo")
  Integer numberDelegatorsAllPoolByEpochNo(@Param("epochNo") Long epochNo);

  /**
   * Get pool delegation summary information by list pool hash id order by pool hash id ascending
   *
   * @return list of pool delegation summary information
   */
  @Query(value =
      "SELECT ph.id as poolView, pod.json as json, pu.pledge as pledge, pu.fixedCost as fee, ph.poolSize as poolSize "
          + "FROM PoolHash ph "
          + "JOIN PoolOfflineData pod ON pod.pool.id = ph.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "WHERE pu.activeEpochNo = "
          + "(SELECT MAX(pu.activeEpochNo) FROM pu.activeEpochNo WHERE pu.poolHash.id = ph.id) AND "
          + "pod.id = (SELECT MAX(pod.id) FROM PoolOfflineData pod WHERE pod.pool.id = ph.id) AND "
          + "ph.poolSize IS NOT NULL "
          + "ORDER BY poolSize DESC ")
  List<PoolDelegationSummaryProjection> findDelegationPoolsSummary(Pageable pageable);
}
