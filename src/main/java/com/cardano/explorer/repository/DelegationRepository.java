package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.custom.DelegatorDataChart;
import com.cardano.explorer.model.response.pool.custom.PoolDetailDelegator;
import com.cardano.explorer.projection.PoolDelegationSummaryProjection;
import com.sotatek.cardano.common.entity.Delegation;
import com.sotatek.cardano.common.entity.Delegation_;
import com.sotatek.cardano.common.entity.Tx;
import java.math.BigDecimal;
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

  @Query("SELECT count(de.id) FROM Delegation de")
  Integer numberDelegatorsAllPool();

  @Query("SELECT count(de.id) FROM Delegation de WHERE de.poolHash.id = :poolId")
  Integer numberDelegatorsByPool(@Param("poolId") Long poolId);

  @EntityGraph(attributePaths = {Delegation_.POOL_HASH, Delegation_.ADDRESS})
  List<Delegation> findByTx(Tx tx);

  @Query(value =
      "SELECT CAST(bk.time AS date) AS time, COUNT(dg.id) AS chartValue FROM PoolHash ph "
          + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
          + "JOIN Block bk ON bk.slotLeader.id = sl.id "
          + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY CAST(bk.time AS date) "
          + "ORDER BY CAST(bk.time AS date) DESC")
  List<DelegatorDataChart> getFiveLastDateDelegatorChart(@Param("poolId") Long poolId,
      Pageable pageable);

  @Query(value = "SELECT COUNT(dg.id) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
      + "WHERE ph.id = :poolId "
      + "GROUP BY CAST(bk.time AS date) "
      + "ORDER BY COUNT(dg.id) DESC")
  List<Long> maxValueDelegatorChart(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT COUNT(dg.id) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
      + "WHERE ph.id = :poolId "
      + "GROUP BY CAST(bk.time AS date) "
      + "ORDER BY COUNT(dg.id) ASC")
  List<Long> minValueDelegatorChart(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT dg.id AS id, sa.hashRaw AS address FROM PoolHash ph "
      + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
      + "JOIN StakeAddress sa ON sa.id = dg.address.id "
      + "WHERE ph.id = :poolId "
      + "ORDER BY dg.id ASC")
  Page<PoolDetailDelegator> getAllDelegatorByPool(@Param("poolId") Long poolId, Pageable pageable);

  @Query(value = "SELECT bk.time AS time, tx.fee AS fee FROM Delegation dg "
      + "JOIN Tx tx ON tx.id = dg.tx.id "
      + "JOIN Block bk ON bk.id = tx.block.id "
      + "WHERE dg.id = :delegatorId")
  PoolDetailDelegator getTimeAndFeeByDelegator(@Param("delegatorId") Long delegatorId);

  @Query(value = "SELECT sum(tu.value) FROM Delegation dg "
      + "JOIN Tx tx ON tx.id = dg.tx.id "
      + "JOIN TxIn ti ON ti.txInput.id = tx.id "
      + "JOIN TxOut tu ON tu.tx.id = ti.txOut.id AND ti.txOutIndex = tu.index "
      + "WHERE dg.id = :delegatorId")
  BigDecimal getTotalValueByDelegator(@Param("delegatorId") Long delegatorId);

  /**
   * Get pool delegation summary information by list pool hash id order by pool hash id ascending
   * @return list of pool delegation summary information
   */
  @Query(value =
      "SELECT ph.id as poolId, pod.json as json, pu.pledge as pledge, pu.fixedCost as fee, SUM(es.amount) as poolSize "
          + "FROM PoolHash ph "
          + "JOIN PoolOfflineData pod ON pod.pool.id = ph.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "JOIN EpochStake es ON es.pool.id = ph.id "
          + "WHERE pu.activeEpochNo = "
          + "(SELECT MAX(pu.activeEpochNo) FROM pu.activeEpochNo WHERE pu.poolHash.id = ph.id) AND "
          + "pod.id = (SELECT MAX(pod.id) FROM PoolOfflineData pod WHERE pod.pool.id = ph.id) "
          + "GROUP BY ph.id, pod.json, pu.pledge, pu.fixedCost "
          + "ORDER BY poolSize DESC ")
  List<PoolDelegationSummaryProjection> findDelegationPoolsSummary(Pageable pageable);
}
