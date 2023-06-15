package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolRetire;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.sql.Timestamp;
import java.util.List;

@Repository
public interface PoolRetireRepository extends JpaRepository<PoolRetire, Long> {
  @Query(value = "SELECT pr.announcedTx.id as txId, pr.retiringEpoch as retiringEpoch, "
          + "ph.view as poolId "
          + "FROM PoolRetire pr "
          + "INNER JOIN PoolHash ph ON pr.poolHash.id = ph.id "
          + "WHERE pr.announcedTx = :tx")
  List<PoolDeRegistrationProjection> findByAnnouncedTx(@Param("tx") Tx tx);

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.blockNo AS blockNo, bk.epochNo AS epochNo, bk.epochSlotNo AS slotNo, "
          + "pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, pu.poolHash.id AS poolId, po.poolName AS poolName, ph.view AS poolView "
          + "FROM PoolRetire pr "
          + "JOIN PoolHash ph ON pr.poolHash.id = ph.id "
          + "JOIN Tx tx ON tx.id = pr.announcedTx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "LEFT JOIN PoolOfflineData po ON pr.poolHash.id  = po.pool.id AND  (po.id is NULL OR po.pmrId = (SELECT max(po2.pmrId) FROM PoolOfflineData po2 WHERE po2.pool.id  = pr.poolHash.id)) "
          + "LEFT JOIN PoolUpdate pu ON pr.poolHash.id = pu.poolHash.id AND (pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pr.poolHash.id  = pu2.poolHash.id)) ")
  Page<TxBlockEpochProjection> getDataForPoolDeRegistration(Pageable pageable);

  @Query(value =
      "SELECT tx.fee AS fee, pr.retiringEpoch AS retiringEpoch, tx.hash AS txHash, bk.time AS time "
          + "FROM PoolRetire pr "
          + "JOIN PoolHash ph ON pr.poolHash.id  = ph.id "
          + "JOIN Tx tx ON pr.announcedTx.id  = tx.id "
          + "JOIN Block bk ON tx.block.id = bk.id "
          + "WHERE ph.view = :poolView "
          + "AND (:txHash IS NULL OR tx.hash = :txHash) "
          + "AND (CAST(:fromDate AS timestamp) IS NULL OR bk.time >= :fromDate) "
          + "AND (CAST(:toDate AS timestamp) IS NULL OR bk.time <= :toDate) ")
  Page<PoolDeRegistrationProjection> getPoolDeRegistration(@Param("poolView") String poolView,
                                                           @Param("txHash") String txHash, @Param("fromDate") Timestamp fromDate,
                                                           @Param("toDate") Timestamp toDate, Pageable pageable);


  @Query(value = "SELECT pr.retiringEpoch FROM PoolRetire pr "
      + "JOIN PoolHash ph ON pr.poolHash.id  = ph.id "
      + "WHERE ph.view = :poolView "
      + "ORDER BY pr.id DESC")
  List<Integer> findByPoolView(@Param("poolView") String poolView);

}
