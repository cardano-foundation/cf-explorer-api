package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.custom.EpochDataChart;
import com.cardano.explorer.model.response.pool.custom.PoolDetailEpoch;
import com.cardano.explorer.model.response.pool.custom.TrxPool;
import com.cardano.explorer.repository.custom.CustomPoolHashRepository;
import com.sotatek.cardano.common.entity.PoolHash;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolHashRepository extends JpaRepository<PoolHash, Long>,
    CustomPoolHashRepository {

  @Query(value = "SELECT count(ph) FROM PoolHash ph")
  Optional<Integer> countPoolHash();

  @Query(value =
      "SELECT bk.epochNo AS epochNo, count(bk.id) AS countBlock FROM PoolHash ph "
          + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
          + "JOIN Block bk ON bk.slotLeader.id = sl.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY bk.epochNo "
          + "ORDER BY bk.epochNo ASC")
  Page<PoolDetailEpoch> findEpochByPool(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT count(DISTINCT bk.epochNo) FROM Block bk WHERE bk.epochNo "
      + "IN (SELECT es.epochNo FROM PoolHash ph JOIN EpochStake es ON es.pool.id = ph.id WHERE ph.id = :poolId)")
  Integer totalEpochByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT pu.fixedCost AS cost, pu.margin AS margin, pu.pledge AS pledge, ph.id AS poolId FROM Block bk "
          + "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id "
          + "JOIN PoolHash ph ON ph.id = sl.poolHash.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "WHERE bk.id = :blockId "
          + "ORDER BY pu.activeEpochNo DESC")
  List<TrxPool> getDataForPoolTx(@Param("blockId") Long blockId);

  @Query(value =
      "SELECT CAST(bk.time AS date) AS time, sum(tu.value) AS chartValue FROM PoolHash ph "
          + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
          + "JOIN Block bk ON bk.slotLeader.id = sl.id "
          + "JOIN Tx tx ON tx.block.id = bk.id "
          + "JOIN TxIn ti ON ti.txInput.id = tx.id "
          + "JOIN TxOut tu ON tu.tx.id = ti.txOut.id AND ti.txOutIndex = tu.index "
          + "WHERE ph.id = :poolId "
          + "GROUP BY CAST(bk.time AS date) "
          + "ORDER BY CAST(bk.time AS date) DESC")
  List<EpochDataChart> getFiveLastDateEpochChart(@Param("poolId") Long poolId,
      Pageable pageable);

  @Query(value = "SELECT sum(tu.value) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "JOIN Tx tx ON tx.block.id = bk.id "
      + "JOIN TxIn ti ON ti.txInput.id = tx.id "
      + "JOIN TxOut tu ON tu.tx.id = ti.txOut.id AND ti.txOutIndex = tu.index "
      + "WHERE ph.id = :poolId "
      + "GROUP BY CAST(bk.time AS date) "
      + "ORDER BY sum(tu.value) DESC")
  List<BigDecimal> maxValueEpochChart(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT sum(tu.value) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "JOIN Tx tx ON tx.block.id = bk.id "
      + "JOIN TxIn ti ON ti.txInput.id = tx.id "
      + "JOIN TxOut tu ON tu.tx.id = ti.txOut.id AND ti.txOutIndex = tu.index "
      + "WHERE ph.id = :poolId "
      + "GROUP BY CAST(bk.time AS date) "
      + "ORDER BY sum(tu.value) ASC")
  List<BigDecimal> minValueEpochChart(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT ph.id FROM Block bk " +
          "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id " +
          "JOIN PoolHash ph ON ph.id = sl.poolHash.id " +
          "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id " +
          "WHERE bk.id IN :blockIds ")
  List<Long> getListPoolIdIn(@Param("blockIds") List<Long> blockIds);
}
