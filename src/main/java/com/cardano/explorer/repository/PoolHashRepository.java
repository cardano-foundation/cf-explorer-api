package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.TxPoolProjection;
import com.cardano.explorer.repository.custom.CustomPoolHashRepository;
import com.sotatek.cardano.common.entity.PoolHash;
import java.math.BigDecimal;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolHashRepository extends JpaRepository<PoolHash, Long>,
    CustomPoolHashRepository {

  @Query(value =
      "SELECT bk.epochNo AS epochNo, count(bk.id) AS countBlock FROM PoolHash ph "
          + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
          + "JOIN Block bk ON bk.slotLeader.id = sl.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY bk.epochNo "
          + "ORDER BY bk.epochNo ASC")
  Page<PoolDetailEpochProjection> findEpochByPool(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value =
      "SELECT pu.fixedCost AS cost, pu.margin AS margin, pu.pledge AS pledge, ph.id AS poolId FROM Block bk "
          + "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id "
          + "JOIN PoolHash ph ON ph.id = sl.poolHash.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "WHERE bk.id = :blockId "
          + "ORDER BY pu.activeEpochNo DESC")
  List<TxPoolProjection> getDataForPoolTx(@Param("blockId") Long blockId);

  @Query(value = "SELECT ph.id FROM PoolHash ph "
      + "LEFT JOIN PoolOfflineData po ON po.pool.id = ph.id "
      + "WHERE (:poolId IS NULL OR ph.id = :poolId) "
      + "GROUP BY ph.id")
  Page<Long> findAllByPoolId(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT ph.id FROM PoolOfflineData po "
      + "LEFT JOIN PoolHash ph ON po.pool.id = ph.id "
      + "WHERE (:poolName IS NULL OR lower(po.json) LIKE :poolName%) "
      + "GROUP BY ph.id")
  Page<Long> findAllByPoolName(@Param("poolName") String poolName, Pageable pageable);

  @Query(value = "SELECT ph.poolSize FROM PoolHash ph WHERE ph.id = :poolId")
  BigDecimal getPoolSizeByPoolId(@Param("poolId") Long poolId);
}
