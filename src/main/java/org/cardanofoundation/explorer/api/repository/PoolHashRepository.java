package org.cardanofoundation.explorer.api.repository;

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
      "SELECT ph.id AS poolId, ph.view AS poolView, po.poolName AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, "
          + "pu.margin AS margin, LENGTH(po.poolName) as poolNameLength "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id = ph.id) "
          + "WHERE :param IS NULL OR ph.view = :param OR LOWER(po.poolName) LIKE CONCAT('%', :param, '%') OR LOWER(po.tickerName) LIKE CONCAT('%', :param, '%') ")
  Page<PoolListProjection> findAllByPoolViewAndPoolName(@Param("param") String param, Pageable pageable);

  @Query(value = "SELECT ph.id FROM PoolHash ph "
      + "WHERE ph.view IN :poolViews ")
  Set<Long> getListPoolIdIn(@Param("poolViews") Set<String> poolViews);

  Optional<PoolHash> findByView(@Param("view") String view);

  @Query(value =
      "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, po.poolName AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, "
          + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.reserves AS reserves, sa.view AS rewardAddress "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id  = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id  = ph.id) "
          + "LEFT JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = :epochNo "
          + "LEFT JOIN AdaPots ap ON ap.epochNo = :epochNo "
          + "WHERE ph.view = :poolView")
  PoolDetailUpdateProjection getDataForPoolDetail(@Param("poolView") String poolView, @Param("epochNo") Integer epochNo);

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
      + "WHERE ph.view = :poolView")
  PoolInfoProjection getPoolInfo(@Param("poolView") String poolView);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, ep.poolDeposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
          + "FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN EpochParam ep ON ep.epochNo = bk.epochNo AND tx.deposit IS NOT NULL AND tx.deposit >= ep.poolDeposit "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE ph.view = :poolView")
  Page<PoolRegistrationProjection> getPoolRegistrationByPool(@Param("poolView") String poolView, Pageable pageable);

  @Query(value = "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView, pod.iconUrl as icon "
      + "FROM PoolHash ph "
      + "INNER JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = "
      + "(SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id ) "
      + "WHERE LOWER(pod.poolName) LIKE CONCAT('%', :query, '%') OR "
      + "LOWER(pod.tickerName) LIKE CONCAT('%', :query, '%') ")
  List<PoolInfoProjection> findByPoolNameLike(@Param("query") String query, Pageable pageable);
}
