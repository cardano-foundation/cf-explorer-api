package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCertificateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.TxBlockEpochProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolUpdate;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long> {

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE ph.id = pu2.poolHash.id) "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY sa.view")
  List<String> findRewardAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE ph.id = pu2.poolHash.id) "
          + "JOIN PoolOwner po ON pu.id = po.poolUpdate.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE ph.id  = :poolId ")
  List<String> findOwnerAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN PoolOwner po ON pu.id = po.poolUpdate.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE ph.view  = :poolView "
          + "GROUP BY sa.view")
  List<String> findOwnerAccountByPoolView(@Param("poolView") String poolView);

  @Query("SELECT pu.id AS poolUpdateId, ph.view AS poolView, pu.pledge AS pledge, "
          + "pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost  AS cost, sa.view AS rewardAccount, "
          + "pmr.url AS metadataUrl, pmr.hash as metadataHash "
          + "FROM PoolUpdate pu "
          + "INNER JOIN PoolHash ph ON pu.poolHash.id = ph.id "
          + "LEFT JOIN PoolMetadataRef pmr ON pu.meta = pmr "
          + "INNER JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.registeredTx = :tx")
  List<PoolUpdateDetailProjection> findByTx(@Param("tx") Tx tx);
  @Query(value =
      "SELECT pu.registeredTxId AS txId, pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, "
          + "pu.poolHash.id AS poolId, po.poolName AS poolName, ph.view AS poolView "
          + "FROM PoolUpdate pu  "
          + "JOIN PoolHash ph ON pu.poolHash.id = ph.id "
          + "LEFT JOIN PoolOfflineData po ON pu.poolHash.id = po.pool.id AND (po.id is NULL OR po.id = "
          + "(SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id  = pu.poolHash.id)) ",
      countQuery = "SELECT count(pu) FROM PoolUpdate pu")
  Page<TxBlockEpochProjection> getDataForPoolRegistration(Pageable pageable);

  @Query(value = "SELECT bk.time FROM PoolUpdate pu "
      + "JOIN Tx t ON pu.registeredTx.id = t.id "
      + "JOIN Block bk ON t.block.id = bk.id "
      + "WHERE pu.id = (SELECT min(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = :poolId) "
      + "AND pu.poolHash.id = :poolId ")
  Timestamp getCreatedTimeOfPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, sa.view AS view FROM PoolUpdate pu "
          + "JOIN PoolOwner po ON pu.id = po.poolUpdate.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE pu.id IN :poolUpdateIds ")
  List<StakeKeyProjection> findOwnerAccountByPoolUpdate(
      @Param("poolUpdateIds") Set<Long> poolUpdateIds);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, tx.hash AS txHash, tx.fee AS fee, bk.time AS time, pu.margin AS margin "
          + "FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id  = tx.id "
          + "JOIN Block bk ON tx.blockId = bk.id AND (tx.deposit IS NULL OR tx.deposit < (SELECT ep.poolDeposit FROM EpochParam ep WHERE ep.epochNo = bk.epochNo)) "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash) "
          + "AND (:txHash IS NULL OR tx.hash = :txHash) "
          + "AND (CAST(:fromDate AS timestamp) IS NULL OR bk.time >= :fromDate) "
          + "AND (CAST(:toDate AS timestamp) IS NULL OR bk.time <= :toDate) ")
  Page<PoolUpdateProjection> findPoolUpdateByPool(@Param("poolViewOrHash") String poolViewOrHash,
      @Param("txHash") String txHash,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      Pageable pageable);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, tx.hash AS txHash, tx.fee AS fee, bk.time AS time, pu.margin AS margin "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx ON pu.registeredTx.id  = tx.id "
          + "JOIN Block bk ON tx.blockId = bk.id "
          + "WHERE tx.id IN :txIds "
          + "AND (:txHash IS NULL OR tx.hash = :txHash) "
          + "AND (CAST(:fromDate AS timestamp) IS NULL OR bk.time >= :fromDate) "
          + "AND (CAST(:toDate AS timestamp) IS NULL OR bk.time <= :toDate) ")
  Page<PoolUpdateProjection> findPoolUpdateByPool(@Param("txIds") Set<Long> txIds,
      @Param("txHash") String txHash,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      Pageable pageable);

  @Query(value =
      "SELECT ph.id AS hashId, ph.hashRaw AS poolId , ph.view AS poolView, pod.poolName AS poolName, " +
              "pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost  AS cost, " +
              "tx.hash AS txHash, bk.time AS time, tx.fee AS fee, sa.view AS rewardAccount, tx.deposit AS deposit "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData pod ON ph.id = pod.pool.id AND pod.id = (SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id) "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id  = sa.id "
          + "WHERE pu.id = :id ")
  PoolUpdateDetailProjection findPoolUpdateDetailById(@Param("id") Long id);
  @Query(value =
      "SELECT sa.view FROM PoolUpdate pu "
          + "JOIN PoolOwner po ON pu.id = po.poolUpdate.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE pu.id  = :id ")
  List<String> findOwnerAccountByPoolUpdate(@Param("id") Long id);

  PoolUpdate findTopByIdLessThanAndPoolHashIdOrderByIdDesc(@Param("id") Long id,
      @Param("poolHashId") Long poolHashId);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, ph.id AS hashId, ph.hashRaw AS poolId , ph.view AS poolView, pod.poolName AS poolName, pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost  AS cost, tx.hash AS txHash, bk.time AS time, tx.fee AS fee, sa.view AS rewardAccount, tx.deposit AS deposit "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData pod ON ph.id = pod.pool.id AND pod.id = (SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id) "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id AND (tx.deposit IS NULL OR tx.deposit < (SELECT ep.poolDeposit FROM EpochParam ep WHERE ep.epochNo = bk.epochNo)) "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id  = sa.id "
          + "WHERE tx.id IN :txIds ")
  Page<PoolUpdateDetailProjection> findPoolUpdateByPool(@Param("txIds") Set<Long> txIds, Pageable pageable);

  @Query("SELECT poolHash.view FROM PoolUpdate poolUpdate "
      + "INNER JOIN PoolHash poolHash ON poolUpdate.poolHash = poolHash "
      + "WHERE poolUpdate.rewardAddr = :stakeAddress "
      + "AND poolUpdate.registeredTx.id = "
      + "(SELECT max(poolUpdate2.registeredTx.id) "
      + "FROM PoolUpdate poolUpdate2 "
      + "WHERE poolUpdate2.poolHash = poolHash) "
      + "AND (SELECT COALESCE(max(poolRetire.retiringEpoch), 0) + 2 "
      + "FROM PoolRetire poolRetire WHERE poolRetire.poolHash = poolHash) < poolUpdate.activeEpochNo")
  List<String> findPoolByRewardAccount(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, tx.hash AS txHash, tx.fee AS fee, bk.time AS time, pu.margin AS margin, ep.poolDeposit AS deposit "
          + "FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id  = tx.id "
          + "JOIN Block bk ON tx.blockId = bk.id "
          + "JOIN EpochParam ep ON ep.epochNo = bk.epochNo AND tx.deposit IS NOT NULL AND tx.deposit >= ep.poolDeposit "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash) "
          + "AND (:txHash IS NULL OR tx.hash = :txHash) "
          + "AND (CAST(:fromDate AS timestamp) IS NULL OR bk.time >= :fromDate) "
          + "AND (CAST(:toDate AS timestamp) IS NULL OR bk.time <= :toDate) ")
  Page<PoolUpdateProjection> findPoolRegistrationByPool(@Param("poolViewOrHash") String poolViewOrHash,
      @Param("txHash") String txHash,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      Pageable pageable);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, tx.hash AS txHash, tx.fee AS fee, bk.time AS time, pu.margin AS margin, ep.poolDeposit AS deposit "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx ON pu.registeredTx.id  = tx.id "
          + "JOIN Block bk ON tx.blockId = bk.id "
          + "JOIN EpochParam ep ON ep.epochNo = bk.epochNo "
          + "WHERE tx.id IN :txIds "
          + "AND (:txHash IS NULL OR tx.hash = :txHash) "
          + "AND (CAST(:fromDate AS timestamp) IS NULL OR bk.time >= :fromDate) "
          + "AND (CAST(:toDate AS timestamp) IS NULL OR bk.time <= :toDate) ")
  Page<PoolUpdateProjection> findPoolRegistrationByPool(@Param("txIds") Set<Long> txIds,
      @Param("txHash") String txHash,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      Pageable pageable);

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE ph.view IN :poolViews "
          + "GROUP BY sa.view")
  List<String> findRewardAccountByPoolView(@Param("poolViews") Set<String> poolViews);

  @Query(value = "SELECT pu FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId "
      + "AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = :poolId)")
  PoolUpdate findLastUpdateByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash) "
          + "GROUP BY sa.view")
  List<String> findRewardAccountByPoolView(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(value =
      "SELECT sa.view FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE ph.id = :poolId "
          + "GROUP BY sa.view")
  List<String> findRewardAccountByPoolId(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT COUNT(pu.id) FROM PoolUpdate pu "
          + "JOIN PoolHash ph ON pu.poolHash.id = ph.id "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash)")
  Integer countPoolUpdateByPool(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(value =
      "SELECT tx.id as txId, tx.hash as txHash, b.epochNo as txEpochNo,"
          + "pu.activeEpochNo as certEpochNo, pu.certIndex as certIndex, pu.id as poolUpdateId, "
          + "b.time as blockTime, b.blockNo as blockNo, b.epochSlotNo as epochSlotNo, b.slotNo as slotNo "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx on pu.registeredTx = tx "
          + "JOIN Block b on tx.block = b "
          + "WHERE pu.poolHash.view = :poolViewOrHash "
          + "OR pu.poolHash.hashRaw = :poolViewOrHash ")
  List<PoolCertificateProjection> getPoolUpdatePoolViewOrHash(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(value =
      "SELECT tx.id as txId, tx.hash as txHash, b.epochNo as txEpochNo,"
          + "pu.activeEpochNo as certEpochNo, pu.certIndex as certIndex, pu.id as poolUpdateId, "
          + "b.time as blockTime, b.blockNo as blockNo, b.epochSlotNo as epochSlotNo, b.slotNo as slotNo "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx on pu.registeredTx = tx "
          + "JOIN Block b on tx.block = b "
          + "WHERE pu.poolHash.view = :poolViewOrHash "
          + "OR pu.poolHash.hashRaw = :poolViewOrHash "
          + "ORDER BY tx.id, pu.certIndex DESC "
          + "LIMIT 1")
  PoolCertificateProjection getLastPoolUpdateByPoolHash(@Param("poolViewOrHash") String poolViewOrHash);
}
