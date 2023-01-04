package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.TxBlockEpochProjection;
import com.sotatek.cardano.common.entity.PoolUpdate;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long> {

  @Query(value =
      "SELECT sa.view FROM PoolUpdate pu JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  List<String> findRewardAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.view FROM PoolOwner po JOIN PoolUpdate pu ON po.poolUpdate.id = pu.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  List<String> findOwnerAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.utxo AS utxo, "
          + "e.fees AS feePerEpoch, ep.influence AS influence, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate "
          + "FROM PoolUpdate pu "
          + "JOIN PoolHash ph ON ph.id = pu.poolHash.id "
          + "JOIN EpochParam ep ON ep.epochNo = ph.epochNo "
          + "JOIN AdaPots ap ON ap.epochNo = ph.epochNo "
          + "JOIN Epoch e ON e.no = ph.epochNo "
          + "WHERE pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId) "
          + "AND ph.id = :poolId")
  PoolDetailUpdateProjection findPoolUpdateByPoolId(@Param("poolId") Long poolId);

  @Query(value = "SELECT pu FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId "
      + "AND pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId)")
  PoolUpdate findLastEpochByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.id AS blockId, bk.epochNo AS epochNo, bk.epochSlotNo AS slotNo, "
          + "pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, pu.poolHash.id AS poolId, po.json AS poolName "
          + "FROM PoolUpdate pu  "
          + "JOIN Tx tx ON tx.id = pu.registeredTx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "LEFT JOIN PoolOfflineData po on pu.poolHash.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id  = pu.poolHash.id)) "
          + "ORDER BY bk.time DESC")
  Page<TxBlockEpochProjection> getDataForPoolRegistration(Pageable pageable);
}
