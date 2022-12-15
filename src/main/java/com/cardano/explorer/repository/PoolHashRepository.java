package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.cardano.explorer.model.response.pool.projection.TxPoolProjection;
import com.sotatek.cardano.common.entity.PoolHash;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolHashRepository extends JpaRepository<PoolHash, Long> {

  @Query(value =
      "SELECT bk.epochNo AS epochNo, count(bk.id) AS countBlock FROM PoolHash ph "
          + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
          + "JOIN Block bk ON bk.slotLeader.id = sl.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY bk.epochNo "
          + "ORDER BY bk.epochNo ASC")
  Page<PoolDetailEpochProjection> findEpochByPool(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value =
      "SELECT pu.fixedCost AS cost, pu.margin AS margin, pu.pledge AS pledge, ph.id AS poolId, po.json AS poolName, bk.id AS blockId FROM Block bk "
          + "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id "
          + "JOIN PoolHash ph ON ph.id = sl.poolHash.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id "
          + "WHERE bk.id IN :blockIds "
          + "AND pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id) "
          + "AND (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id = ph.id))")
  List<TxPoolProjection> getDataForPoolTx(@Param("blockIds") Set<Long> blockIds);

  @Query(value =
      "SELECT ph.view AS poolView, po.json AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, ph.poolSize AS poolSize, ep.optimalPoolCount AS paramK "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "LEFT JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "WHERE (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id = ph.id)) "
          + "AND (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND (:poolId IS NULL OR ph.id = :poolId) "
          + "ORDER BY ph.id ASC")
  Page<PoolListProjection> findAllByPoolId(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value =
      "SELECT ph.view AS poolView, po.json AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, ph.poolSize AS poolSize, ep.optimalPoolCount AS paramK "
          + "FROM PoolOfflineData po "
          + "JOIN PoolHash ph ON ph.id = po.pool.id "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "WHERE (po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id = ph.id)) "
          + "AND (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND (:poolName IS NULL OR lower(po.json) LIKE :poolName%) "
          + "ORDER BY ph.id ASC")
  Page<PoolListProjection> findAllByPoolName(@Param("poolName") String poolName, Pageable pageable);

  @Query(value = "SELECT ph.id FROM Block bk " +
      "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id " +
      "JOIN PoolHash ph ON ph.id = sl.poolHash.id " +
      "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id " +
      "WHERE bk.id IN :blockIds ")
  List<Long> getListPoolIdIn(@Param("blockIds") List<Long> blockIds);

  Optional<PoolHash> findByView(String view);
}
