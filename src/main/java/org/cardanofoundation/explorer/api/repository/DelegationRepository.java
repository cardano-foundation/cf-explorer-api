package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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
  List<Delegation> findByTx(@Param("tx") Tx tx);

  @Query(value =
      "SELECT dg.activeEpochNo AS chartKey, count(dg.id) AS chartValue FROM Delegation dg "
          + "WHERE dg.poolHash.id = :poolId "
          + "GROUP BY dg.activeEpochNo "
          + "ORDER BY dg.activeEpochNo ASC")
  List<DelegatorChartProjection> getDataForDelegatorChart(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.id AS stakeAddressId, sa.view AS view , bk.time AS time, tx.fee AS fee "
          + "FROM PoolHash ph "
          + "JOIN Delegation dg ON dg.poolHash.id = ph.id "
          + "JOIN StakeAddress sa ON sa.id = dg.address.id "
          + "JOIN StakeRegistration sr ON sa.id = sr.addr.id AND sr.id = (SELECT max(sr2.id) FROM StakeRegistration sr2 WHERE sa.id = sr2.addr.id) "
          + "LEFT JOIN StakeDeregistration sd ON sa.id = sd.addr.id AND sd.id IS NULL "
          + "JOIN Tx tx ON tx.id = sr.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY sa.id, sa.view, bk.time, tx.fee "
          + "ORDER BY bk.time DESC")
  Page<PoolDetailDelegatorProjection> getAllDelegatorByPool(@Param("poolId") Long poolId,
      Pageable pageable);

  @Query("SELECT count(de.id) FROM Delegation de WHERE de.activeEpochNo = :epochNo")
  Integer numberDelegatorsAllPoolByEpochNo(@Param("epochNo") Long epochNo);

  /**
   * Get pool delegation summary information by list pool hash id order by pool hash id ascending
   *
   * @return list of pool delegation summary information
   */
  @Query(value =
      "SELECT ph.id AS poolId, ph.view AS poolView, pod.poolName AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee,"
          + "ph.poolSize AS poolSize, ep.optimalPoolCount AS optimalPoolCount, "
          + "pu.margin AS margin, ad.reserves AS reserves "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData pod ON pod.pool.id = ph.id "
          + "LEFT JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = ph.epochNo "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = ph.epochNo "
          + "WHERE pu.id = "
          + "(SELECT MAX(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = ph.id) AND "
          + "pod.pmrId = (SELECT MAX(pod2.pmrId) FROM PoolOfflineData pod2 WHERE pod2.pool.id = ph.id) AND "
          + "ph.poolSize IS NOT NULL "
          + "ORDER BY poolSize DESC ")
  List<PoolDelegationSummaryProjection> findDelegationPoolsSummary(Pageable pageable);

  @Query("SELECT delegation.tx.id"
      + " FROM Delegation delegation"
      + " WHERE delegation.address = :stakeKey AND delegation.tx.id IN :txIds")
  List<Long> findDelegationByAddressAndTxIn(@Param("stakeKey") StakeAddress stakeKey,
                                            @Param("txIds") Collection<Long> txIds);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, poolHash.view as poolId,"
      + " poolOfflineData.json as poolData, poolOfflineData.tickerName as tickerName"
      + " FROM Delegation delegation"
      + " INNER JOIN Tx tx ON delegation.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON delegation.address = stake"
      + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
      + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.pmrId ="
      + " (SELECT max(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeDelegationProjection> findDelegationByAddress(@Param("stakeKey") String stakeKey, Pageable pageable);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, tx.fee as fee, tx.outSum as outSum"
      + " FROM Delegation delegation"
      + " INNER JOIN Tx tx ON delegation.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " WHERE delegation.address = :stakeKey"
      + " AND (block.time >= :fromTime ) "
      + " AND (block.time <= :toTime)"
      + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeDelegationProjection> findDelegationByAddress(@Param("stakeKey") StakeAddress stakeKey,
                                                          @Param("txHash") String txHash,
                                                          @Param("fromTime") Timestamp fromTime,
                                                          @Param("toTime") Timestamp toTime,
                                                          Pageable pageable);
  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, poolHash.view as poolId,"
      + " poolOfflineData.json as poolData, poolOfflineData.tickerName as tickerName,"
      + " tx.fee as fee, tx.outSum as outSum"
      + " FROM Delegation delegation"
      + " INNER JOIN Tx tx ON delegation.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
      + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.pmrId ="
      + " (SELECT max(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
      + " WHERE delegation.address = :stakeKey AND tx.hash = :txHash")
  Optional<StakeDelegationProjection> findDelegationByAddressAndTx(@Param("stakeKey") StakeAddress stakeKey,
                                                                   @Param("txHash") String txHash);

  @Query("SELECT poolHash.view as poolId, poolOfflineData.json as poolData,"
      + " poolOfflineData.tickerName as tickerName"
      + " FROM Delegation delegation"
      + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
      + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.pmrId ="
      + " (SELECT max(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
      + " WHERE delegation.id = (SELECT max(d2.id) FROM Delegation d2 where d2.address = :address )")
  Optional<StakeDelegationProjection> findPoolDataByAddress(@Param("address") StakeAddress address);

  @Query("SELECT poolHash.view as poolId, poolOfflineData.json as poolData,"
      + " poolOfflineData.tickerName as tickerName, stake.view as stakeAddress"
      + " FROM Delegation delegation"
      + " INNER JOIN StakeAddress stake ON stake.id = delegation.address.id"
      + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
      + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.pmrId ="
      + " (SELECT max(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
      + " WHERE delegation.id IN "
      + " (SELECT max(d.id) FROM Delegation d "
      + " INNER JOIN StakeAddress sa ON d.address = sa"
      + " WHERE sa.view IN :addresses"
      + " GROUP BY sa.view )")
  List<StakeDelegationProjection> findPoolDataByAddressIn(@Param("addresses") Set<String> addresses);

  @Query("SELECT sum(txo.value) "
      + "FROM TxOut txo "
      + "LEFT JOIN TxIn txi ON txo.tx.id = txi.txOut.id AND txo.index = txi.txOutIndex "
      + "LEFT JOIN Tx tx ON tx.id = txo.tx.id "
      + "LEFT JOIN Block block ON tx.block.id = block.id "
      + "WHERE (txi.txInput.id IS NULL) AND (block.epochNo IS NOT NULL) "
      + "AND txo.stakeAddress.id IN (SELECT d1.address.id "
      + "FROM Delegation d1, PoolHash ph "
      + "WHERE ph.id = d1.poolHash.id "
      + "AND ph.view = :poolView "
      + "AND NOT EXISTS "
      + "(SELECT TRUE "
      + "FROM Delegation d2 "
      + "WHERE d2.address.id = d1.address.id "
      + "AND d2.tx.id > d1.tx.id) "
      + "AND NOT EXISTS "
      + "(SELECT TRUE "
      + "FROM StakeDeregistration sd "
      + "WHERE sd.addr.id = d1.address.id "
      + "AND sd.tx.id > d1.tx.id))")
  BigInteger findDelegateStakeByPool(@Param("poolView") String poolView);

  @Query(value =
      "SELECT ph.id AS poolId, ph.view AS poolView, pod.poolName AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee,"
          + "ep.optimalPoolCount AS optimalPoolCount, "
          + "pu.margin AS margin, ad.reserves AS reserves "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData pod ON pod.pool.id = ph.id "
          + "LEFT JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "WHERE pu.id = "
          + "(SELECT MAX(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = ph.id) AND "
          + "pod.id = (SELECT MAX(pod2.id) FROM PoolOfflineData pod2 WHERE pod2.pool.id = ph.id) AND "
          + "ph.id IN :poolIds")
  List<PoolDelegationSummaryProjection> findDelegationPoolsSummary(@Param("poolIds") Set<Long> poolIds);
}
