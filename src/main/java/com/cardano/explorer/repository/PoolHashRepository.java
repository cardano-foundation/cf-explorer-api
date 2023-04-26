package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.PoolInfoProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.cardano.explorer.model.response.pool.projection.PoolRegistrationProjection;
import com.sotatek.cardano.common.entity.PoolHash;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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
      "SELECT ph.id AS poolId, ph.view AS poolView, po.poolName AS poolName, pu.pledge AS pledge, pu.fixedCost AS fee, ep.optimalPoolCount AS paramK, "
          + "pu.margin AS margin, ad.reserves AS reserves "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "LEFT JOIN AdaPots ad ON ad.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "WHERE (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id = ph.id)) "
          + "AND (pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = ph.id)) "
          + "AND ((:poolView IS NULL OR ph.view = :poolView) "
          + "OR (:poolName IS NULL OR po.poolName LIKE %:poolName%)) ")
  Page<PoolListProjection> findAllByPoolViewAndPoolName(@Param("poolView") String poolView,
      @Param("poolName") String poolName, Pageable pageable);

  @Query(value = "SELECT ph.id FROM Block bk " +
      "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id " +
      "JOIN PoolHash ph ON ph.id = sl.poolHash.id " +
      "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id " +
      "WHERE bk.id IN :blockIds ")
  List<Long> getListPoolIdIn(@Param("blockIds") List<Long> blockIds);

  Optional<PoolHash> findByView(String view);

  @Query(value =
      "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, po.poolName AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, "
          + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.reserves AS reserves, sa.view AS rewardAddress "
          + "FROM PoolHash ph "
          + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po.id) FROM PoolOfflineData po WHERE po.pool.id  = ph.id)) "
          + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id  = ph.id) "
          + "LEFT JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "LEFT JOIN EpochParam ep ON ep.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "LEFT JOIN AdaPots ap ON ap.epochNo = (SELECT max(e.no) FROM Epoch e) "
          + "WHERE ph.view = :poolView ")
  PoolDetailUpdateProjection getDataForPoolDetail(@Param("poolView") String poolView);

  @Query(value = "SELECT ph.view FROM PoolHash ph")
  List<String> findAllView();

  @Query(value =
      "SELECT pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, ep.poolDeposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
          + "FROM PoolUpdate pu "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.id = :id")
  PoolRegistrationProjection getPoolRegistration(@Param("id") Long id);

  @Query(value = "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView "
      + "FROM PoolHash ph "
      + "LEFT JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = (SELECT max(pod.id) FROM PoolOfflineData pod WHERE ph.id = pod.pool.id ) "
      + "WHERE ph.view = :poolView")
  PoolInfoProjection getPoolInfo(@Param("poolView") String poolView);

  @Query(value =
      "SELECT pu.id AS poolUpdateId, pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, ep.poolDeposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
          + "FROM PoolHash ph "
          + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id "
          + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
          + "JOIN Block bk ON tx.block.id  = bk.id "
          + "JOIN EpochParam ep ON pu.activeEpochNo = ep.epochNo "
          + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE ph.view = :poolView")
  Page<PoolRegistrationProjection> getPoolRegistrationByPool(@Param("poolView") String poolView, Pageable pageable);
}
