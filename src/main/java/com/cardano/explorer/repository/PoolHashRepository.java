package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.sotatek.cardano.common.entity.PoolHash;
import java.util.List;
import java.util.Optional;
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
      "SELECT ph.view AS poolView, po.json AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, ph.poolSize AS poolSize, ep.optimalPoolCount AS paramK, e.blkCount AS blkCount, ep.maxBlockSize AS maxBlockSize "
          + ", ad.utxo AS utxo, pu.margin AS margin, e.fees AS feePerEpoch, ep.influence AS influence, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "LEFT JOIN EpochParam ep ON ph.epochNo = ep.epochNo "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = ph.epochNo "
          + "LEFT JOIN Epoch e ON e.no = ph.epochNo "
          + "WHERE (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id = ph.id)) "
          + "AND (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND ((:poolView IS NULL OR ph.view = :poolView) "
          + "OR (:poolName IS NULL OR po.json LIKE :poolName%)) "
          + "ORDER BY ph.id ASC")
  Page<PoolListProjection> findAllByPoolViewAndPoolName(@Param("poolView") String poolView,
      @Param("poolName") String poolName, Pageable pageable);

  @Query(value = "SELECT ph.id FROM Block bk " +
      "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id " +
      "JOIN PoolHash ph ON ph.id = sl.poolHash.id " +
      "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id " +
      "WHERE bk.id IN :blockIds ")
  List<Long> getListPoolIdIn(@Param("blockIds") List<Long> blockIds);

  Optional<PoolHash> findByView(String view);

  @Query(value =
      "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, ph.poolSize AS poolSize, po.json AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, e.blkCount AS blkCount, ep.maxBlockSize AS maxBlockSize, "
          + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.utxo AS utxo, e.fees AS feePerEpoch, ep.influence AS influence, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id  = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id  = ph.id) "
          + "LEFT JOIN EpochParam ep ON ph.epochNo = ep.epochNo "
          + "LEFT JOIN AdaPots ap ON ph.epochNo = ap.epochNo "
          + "LEFT JOIN Epoch e ON ph.epochNo = e.no "
          + "WHERE ph.view = :poolView ")
  PoolDetailUpdateProjection getDataForPoolDetail(@Param("poolView") String poolView);

  @Query(value =
      "SELECT ph.view AS poolView, pu.pledge AS pledge, pu.fixedCost AS fee, ph.poolSize AS poolSize, ep.optimalPoolCount AS paramK, e.blkCount AS blkCount, ep.maxBlockSize AS maxBlockSize "
          + ", ad.utxo AS utxo, pu.margin AS margin, e.fees AS feePerEpoch, ep.influence AS influence, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "LEFT JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = pu.activeEpochNo "
          + "LEFT JOIN Epoch e ON e.no = pu.activeEpochNo "
          + "WHERE (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND (ph.view IN :poolViews) ")
  List<PoolListProjection> findDataCalculateReward(@Param("poolViews") List<String> poolViews);

  @Query(value =
      "SELECT ph.view AS poolView, pu.pledge AS pledge, pu.fixedCost AS fee, ph.poolSize AS poolSize, ep.optimalPoolCount AS paramK, e.blkCount AS blkCount, ep.maxBlockSize AS maxBlockSize "
          + ", ad.utxo AS utxo, pu.margin AS margin, e.fees AS feePerEpoch, ep.influence AS influence, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "LEFT JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = pu.activeEpochNo "
          + "LEFT JOIN Epoch e ON e.no = pu.activeEpochNo "
          + "WHERE (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND (ph.view = :poolView) ")
  PoolListProjection findDataCalculateReward(@Param("poolView") String poolView);
}
