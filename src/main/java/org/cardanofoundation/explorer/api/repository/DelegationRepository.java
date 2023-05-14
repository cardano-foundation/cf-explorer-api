package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
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
          + "JOIN StakeRegistration sr ON sa.id = sr.addr.id AND sr.id = (SELECT max(sr.id) FROM StakeRegistration sr WHERE sa.id = sr.addr.id) "
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
      "SELECT ph.view AS poolView, pod.json AS json, pu.pledge AS pledge, pu.fixedCost AS fee,"
          + " ph.poolSize AS poolSize, ep.optimalPoolCount AS optimalPoolCount, "
          + "ad.utxo AS utxo, pu.margin AS margin, e.fees AS feePerEpoch, ep.influence AS influence, "
          + "ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate, e.blkCount AS blkCount, ep.maxBlockSize AS maxBlockSize "
          + "FROM PoolHash ph "
          + "JOIN PoolOfflineData pod ON pod.pool.id = ph.id "
          + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
          + "JOIN EpochParam ep ON ep.epochNo = ph.epochNo "
          + "JOIN AdaPots ad ON ad.epochNo = ph.epochNo "
          + "JOIN Epoch e ON e.no = ph.epochNo "
          + "WHERE pu.activeEpochNo = "
          + "(SELECT MAX(pu.activeEpochNo) FROM pu.activeEpochNo WHERE pu.poolHash.id = ph.id) AND "
          + "pod.pmrId = (SELECT MAX(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool.id = ph.id) AND "
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
  Page<StakeDelegationProjection> findDelegationByAddress(@Param("stakeKey") String stakeKey,
                                                          Pageable pageable);

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
  Optional<StakeDelegationProjection> findDelegationByAddressAndTx(
      @Param("stakeKey") StakeAddress stakeKey, @Param("txHash") String txHash);

  @Query("SELECT poolHash.view as poolId, poolOfflineData.json as poolData,"
      + " poolOfflineData.tickerName as tickerName"
      + " FROM Delegation delegation"
      + " INNER JOIN PoolHash poolHash ON delegation.poolHash = poolHash"
      + " LEFT JOIN PoolOfflineData poolOfflineData ON poolOfflineData.pmrId ="
      + " (SELECT max(pod.pmrId) FROM PoolOfflineData pod WHERE pod.pool = poolHash)"
      + " WHERE delegation.id = (SELECT max(id) FROM Delegation where address = :address )")
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

}
