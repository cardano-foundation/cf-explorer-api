package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import com.sotatek.cardano.common.entity.PoolRetire;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolRetireRepository extends JpaRepository<PoolRetire, Long> {

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.blockNo AS blockNo, bk.epochNo AS epochNo, bk.epochSlotNo AS slotNo, "
          + "pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, pu.poolHash.id AS poolId, po.json AS poolName, ph.view AS poolView "
          + "FROM PoolRetire pr "
          + "JOIN PoolHash ph ON pr.poolHash.id = ph.id "
          + "JOIN Tx tx ON tx.id = pr.announcedTx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "LEFT JOIN PoolOfflineData po ON pr.poolHash.id  = po.pool.id AND  (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id  = pr.poolHash.id)) "
          + "LEFT JOIN PoolUpdate pu ON pr.poolHash.id = pu.poolHash.id AND (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pr.poolHash.id  = pu.poolHash.id)) "
          + "ORDER BY bk.time DESC")
  Page<TxBlockEpochProjection> getDataForPoolDeRegistration(Pageable pageable);

}
