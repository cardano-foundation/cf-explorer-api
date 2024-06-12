package org.cardanofoundation.explorer.api.repository.ledgersync;

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

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.PoolOverviewProjection;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.*;

@Repository
public interface DelegationRepository extends JpaRepository<Delegation, Long> {

  @EntityGraph(attributePaths = {Delegation_.POOL_HASH, Delegation_.ADDRESS})
  List<Delegation> findByTx(@Param("tx") Tx tx);

  @Query(
      value =
          "SELECT sa.id AS stakeAddressId, sa.view AS view , bk.time AS time, tx.fee AS fee "
              + "FROM StakeAddress sa "
              + "JOIN StakeRegistration sr ON sa.id = sr.addr.id AND sr.id = (SELECT max(sr2.id) FROM StakeRegistration sr2 WHERE sa.id = sr2.addr.id) "
              + "JOIN Tx tx ON sr.tx.id  = tx.id "
              + "JOIN Block bk ON tx.block.id = bk.id "
              + "WHERE sa.id IN :addressIds "
              + "ORDER BY tx.id DESC")
  List<PoolDetailDelegatorProjection> getDelegatorsByAddress(
      @Param("addressIds") Set<Long> addressIds);

  /**
   * Get pool delegation summary information by list pool hash id order by pool hash id ascending
   *
   * @return list of pool delegation summary information
   */
  @Query(
      value =
          "SELECT ph.id AS poolId, ph.view AS poolView, pod.poolName AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, pu.margin AS margin "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData pod ON pod.pool.id = ph.id AND pod.id = (SELECT MAX(pod2.id) FROM PoolOfflineData pod2 WHERE pod2.pool.id = ph.id) "
              + "LEFT JOIN PoolUpdate pu ON pu.poolHash.id = ph.id AND pu.id = (SELECT MAX(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = ph.id) "
              + "WHERE ph.id IN :poolIds")
  List<PoolDelegationSummaryProjection> findDelegationPoolsSummary(
      @Param("poolIds") Set<Long> poolIds);


  @Query(
      "SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo, block.slotNo as slotNo,"
          + " block.blockNo as blockNo, block.epochNo as epochNo, poolHash.view as poolId,"
          + " poolOfflineData.json as poolData, poolOfflineData.tickerName as tickerName"
          + " FROM Delegation delegation"
          + " INNER JOIN Tx tx ON delegation.tx = tx"
          + " INNER JOIN Block block ON tx.block = block"
          + " INNER JOIN StakeAddress stake ON delegation.address = stake"
          + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
          + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.id ="
          + " (SELECT max(pod.id) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
          + " WHERE stake.view = :stakeKey"
          + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeDelegationProjection> findDelegationByAddress(
      @Param("stakeKey") String stakeKey, Pageable pageable);

  @Query(
      "SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo, "
          + " poolOfflineData.poolName as poolName, delegation.poolHash.view as poolId,"
          + " block.blockNo as blockNo, block.epochNo as epochNo, tx.fee as fee, tx.outSum as outSum"
          + " FROM Delegation delegation"
          + " INNER JOIN Tx tx ON delegation.tx = tx"
          + " INNER JOIN Block block ON tx.block = block"
          + " LEFT JOIN PoolOfflineData poolOfflineData on poolOfflineData.poolId = delegation.poolHash.id"
          + " WHERE delegation.address = :stakeKey"
          + " AND (block.time >= :fromTime ) "
          + " AND (block.time <= :toTime)"
          + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeDelegationProjection> findDelegationByAddress(
      @Param("stakeKey") StakeAddress stakeKey,
      @Param("txHash") String txHash,
      @Param("fromTime") Timestamp fromTime,
      @Param("toTime") Timestamp toTime,
      Pageable pageable);

  @Query(
      "SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
          + " block.blockNo as blockNo, block.epochNo as epochNo, poolHash.view as poolId,"
          + " poolOfflineData.poolName as poolData, poolOfflineData.tickerName as tickerName,"
          + " tx.fee as fee, tx.outSum as outSum"
          + " FROM Delegation delegation"
          + " INNER JOIN Tx tx ON delegation.tx = tx"
          + " INNER JOIN Block block ON tx.block = block"
          + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
          + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.id ="
          + " (SELECT max(pod.id) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
          + " WHERE delegation.address = :stakeKey AND tx.hash = :txHash")
  Optional<StakeDelegationProjection> findDelegationByAddressAndTx(
      @Param("stakeKey") StakeAddress stakeKey, @Param("txHash") String txHash);

  @Query(
      "SELECT poolHash.view as poolId, poolOfflineData.poolName as poolData,"
          + " poolOfflineData.tickerName as tickerName, poolOfflineData.logoUrl as logoUrl, poolOfflineData.iconUrl as iconUrl "
          + " FROM Delegation delegation"
          + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
          + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.id ="
          + " (SELECT max(pod.id) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
          + " WHERE delegation.id = (SELECT max(d2.id) FROM Delegation d2 where d2.address = :address )"
          + " AND (SELECT max(sr.txId) FROM StakeRegistration sr WHERE sr.addr = :address) >"
          + " (SELECT COALESCE(max(sd.txId), 0) FROM StakeDeregistration sd WHERE sd.addr = :address)")
  Optional<StakeDelegationProjection> findPoolDataByAddress(@Param("address") StakeAddress address);

  @Query(
      "SELECT poolHash.view as poolId, poolOfflineData.poolName as poolData,"
          + " poolOfflineData.tickerName as tickerName, stake.view as stakeAddress"
          + " FROM Delegation delegation"
          + " INNER JOIN StakeAddress stake ON stake.id = delegation.address.id"
          + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
          + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.id ="
          + " (SELECT max(pod.id) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
          + " WHERE delegation.id IN "
          + " (SELECT max(d.id) FROM Delegation d "
          + " INNER JOIN StakeAddress sa ON d.address = sa"
          + " WHERE sa.view IN :addresses"
          + " GROUP BY sa.view )")
  List<StakeDelegationProjection> findPoolDataByAddressIn(
      @Param("addresses") Set<String> addresses);

  Boolean existsByAddress(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(
      value =
          "SELECT dg1.address.id "
              + "FROM Delegation dg1 "
              + "JOIN PoolHash ph ON dg1.poolHash.id = ph.id "
              + "WHERE (ph.view = :poolViewOrHash "
              + "OR ph.hashRaw = :poolViewOrHash) "
              + "AND NOT EXISTS "
              + "(SELECT TRUE "
              + "FROM Delegation dg2 "
              + "WHERE dg2.address.id = dg1.address.id "
              + "AND dg2.tx.id > dg1.tx.id) "
              + "AND NOT EXISTS "
              + "(SELECT TRUE "
              + "FROM StakeDeregistration sd "
              + "WHERE sd.addr.id = dg1.address.id "
              + "AND sd.tx.id > dg1.tx.id)")
  Page<Long> liveDelegatorsList(@Param("poolViewOrHash") String poolViewOrHash, Pageable pageable);

  @Query(
      value = "SELECT DISTINCT delegation.txId FROM Delegation delegation",
      countQuery = "SELECT COUNT(DISTINCT delegation.txId) FROM Delegation delegation")
  Page<Long> findAllDelegations(Pageable pageable);

  @Query(
      value =
          "SELECT delegation.address.view as stakeAddress, poolHash.view as poolView,"
              + " po.tickerName as tickerName, po.poolName as poolName, delegation.txId as txId"
              + " FROM Delegation delegation"
              + " INNER JOIN StakeAddress stake ON stake.id = delegation.stakeAddressId"
              + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
              + " LEFT JOIN PoolOfflineData po ON poolHash.id = po.pool.id "
              + " AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id = poolHash.id))"
              + " WHERE delegation.txId IN :txIds")
  List<DelegationProjection> findDelegationByTxIdIn(@Param("txIds") List<Long> txIds);

  @Query(
      value =
          """
                select distinct(sa.view) from Delegation d
                      join StakeAddress sa on sa.id = d.stakeAddressId
                      where d.poolHash.view in :poolHashes
              """)
  Set<String> getStakeAddressDelegatorsByPoolIds(@Param("poolHashes") Collection<String> poolHashes);
}
