package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
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
          + "WHERE ph.id = :poolId AND bk.epochNo IN :epochNo "
          + "GROUP BY bk.epochNo")
  List<PoolDetailEpochProjection> findEpochByPool(@Param("poolId") Long poolId, @Param("epochNo") Set<Integer> epochNo);

  @Query(value =
      "SELECT ph.id AS poolId, ph.view AS poolView, po.pool_name AS poolName, pu.pledge AS pledge, pu.fixed_cost AS fee, "
          + "po.ticker_name as tickerName, pu.margin AS margin, LENGTH(po.pool_name) as poolNameLength, "
          + "COALESCE(ap.delegator_cnt, 0) as numberDelegators, COALESCE(ap.block_in_epoch, 0) as epochBlock, "
          + "COALESCE(ap.block_life_time, 0) as lifetimeBlock, "
          + "COALESCE(pi.active_stake, 0) AS poolSize, COALESCE(pi.live_saturation, 0) AS saturation "
          + "FROM pool_hash ph "
          + "LEFT JOIN pool_offline_data po ON ph.id = po.pool_id AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM pool_offline_data po2 WHERE po2.pool_id = ph.id)) "
          + "LEFT JOIN pool_update pu ON ph.id = pu.hash_id AND pu.id = (SELECT max(pu2.id) FROM pool_update pu2 WHERE pu2.hash_id = ph.id) "
          + "LEFT JOIN agg_pool_info ap ON ph.id = ap.pool_id "
          + "LEFT JOIN pool_info pi ON ph.id = pi.pool_id AND pi.fetched_at_epoch = :epochNo "
          + "WHERE ph.id NOT IN :exceptPoolIds AND ( :param IS NULL OR ph.view = :param "
          + "OR ph.hash_raw = :param OR LOWER(po.pool_name) LIKE CONCAT('%', :param, '%') OR LOWER(po.ticker_name) LIKE CONCAT('%', :param, '%') )",
      countQuery = "SELECT COUNT(*) FROM "
          + "(SELECT 1 FROM pool_hash ph "
          + "LEFT JOIN pool_offline_data po ON ph.id = po.pool_id AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM pool_offline_data po2 WHERE po2.pool_id = ph.id)) "
          + "WHERE ph.id NOT IN :exceptPoolIds AND ( ph.view = :param "
          + "OR ph.hash_raw = :param "
          + "OR LOWER(po.pool_name) LIKE CONCAT('%', :param, '%') OR LOWER(po.ticker_name) LIKE CONCAT('%', :param, '%') ) "
          + "AND :epochNo IS NOT NULL "
          + "LIMIT 1000) AS A",
      nativeQuery = true)
  Page<PoolListProjection> findAllByPoolViewOrPoolNameOrPoolHash(@Param("param") String param,
                                                                 @Param("exceptPoolIds") Collection<Long> exceptPoolIds,
                                                                 @Param("epochNo") Integer epochNo,
                                                                 Pageable pageable);

  @Query(value =
      "SELECT ph.id AS poolId, ph.view AS poolView, po.poolName AS poolName, "
          + "po.tickerName as tickerName, pu.pledge AS pledge, pu.fixedCost AS fee, "
          + "pu.margin AS margin, LENGTH(po.poolName) as poolNameLength, "
          + "COALESCE(ap.delegatorCount, 0) as numberDelegators, COALESCE(ap.blockInEpoch, 0) as epochBlock, "
          + "COALESCE(ap.blockLifeTime, 0) as lifetimeBlock, "
          + "COALESCE(pi.activeStake, 0) AS poolSize, COALESCE(pi.liveSaturation, 0) AS saturation "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = ph.id) "
          + "LEFT JOIN AggregatePoolInfo ap ON ap.poolHash = ph "
          + "LEFT JOIN PoolInfo pi ON pi.pool = ph AND pi.fetchedAtEpoch = :epochNo "
          + "WHERE ph.id NOT IN :exceptPoolIds ",
      countQuery = "SELECT COUNT(ph.id) FROM PoolHash ph WHERE ph.id NOT IN :exceptPoolIds AND :epochNo IS NOT NULL")
  Page<PoolListProjection> findAllWithoutQueryParam(@Param("exceptPoolIds") Collection<Long> exceptPoolIds,
                                                    @Param("epochNo") Integer epochNo,
                                                    Pageable pageable);

  @Query(value = "SELECT ph.id FROM PoolHash ph "
      + "WHERE ph.view IN :poolViews ")
  Set<Long> getListPoolIdIn(@Param("poolViews") Set<String> poolViews);

  @Query(value = "SELECT ph FROM PoolHash ph "
      + "WHERE (ph.view = :poolViewOrHash "
      + "OR ph.hashRaw = :poolViewOrHash)")
  Optional<PoolHash> findByViewOrHashRaw(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(value =
      "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, po.poolName AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, "
          + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.reserves AS reserves, sa.view AS rewardAddress, "
          + "api.delegatorCount as delegators, api.blockLifeTime as lifetimeBlock, api.blockInEpoch as epochBlock, api.updateTime as lastUpdate, "
          + "po.json as json, po.iconUrl AS iconUrl, po.logoUrl AS logoUrl, ph.view AS poolView "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id  = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id  = ph.id) "
          + "LEFT JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = :epochNo "
          + "LEFT JOIN AdaPots ap ON ap.epochNo = :epochNo "
          + "LEFT JOIN AggregatePoolInfo api ON api.poolHash = ph "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash)")
  PoolDetailUpdateProjection getDataForPoolDetail(@Param("poolViewOrHash") String poolViewOrHash, @Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, tx.deposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.id = :id")
  PoolRegistrationProjection getPoolRegistration(@Param("id") Long id);

  @Query(value = "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView, pod.iconUrl as icon "
      + "FROM PoolHash ph "
      + "LEFT JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = (SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id ) "
      + "WHERE (ph.view = :poolViewOrHash "
      + "OR ph.hashRaw = :poolViewOrHash)")
  PoolInfoProjection getPoolInfo(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, ep.poolDeposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
          + "FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN EpochParam ep ON ep.epochNo = bk.epochNo AND tx.deposit IS NOT NULL AND tx.deposit >= ep.poolDeposit "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE (ph.view = :poolViewOrHash "
          + "OR ph.hashRaw = :poolViewOrHash) ")
  Page<PoolRegistrationProjection> getPoolRegistrationByPool(@Param("poolViewOrHash") String poolViewOrHash,
                                                             Pageable pageable);

  @Query(value = "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView, pod.iconUrl as icon "
      + "FROM PoolHash ph "
      + "INNER JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = "
      + "(SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id ) "
      + "WHERE LOWER(pod.poolName) LIKE CONCAT('%', :query, '%') OR "
      + "LOWER(pod.tickerName) LIKE CONCAT('%', :query, '%') ")
  List<PoolInfoProjection> findByPoolNameLike(@Param("query") String query, Pageable pageable);
}
