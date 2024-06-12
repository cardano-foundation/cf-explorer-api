package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import org.cardanofoundation.explorer.api.projection.PoolOverviewProjection;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolHash;

@Repository
public interface PoolHashRepository extends JpaRepository<PoolHash, Long> {

  @Query(
      value =
          "SELECT bk.epochNo AS epochNo, count(bk.id) AS countBlock FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE ph.id = :poolId AND bk.epochNo IN :epochNo "
              + "GROUP BY bk.epochNo")
  List<PoolDetailEpochProjection> findEpochByPool(
      @Param("poolId") Long poolId, @Param("epochNo") Set<Integer> epochNo);

  @Query(
      value =
          " SELECT ph.id AS poolId, ph.view AS poolView, po.poolName AS poolName, pu.pledge AS pledge, "
              + "po.tickerName as tickerName, LENGTH(po.poolName) as poolNameLength, "
              + "COALESCE(api.governanceParticipationRate, -1) as governanceParticipationRate, COALESCE(api.votingPower, -1) as votingPower,"
              + "COALESCE(api.blockLifeTime, 0) as lifetimeBlock, COALESCE(api.blockInEpoch, 0) as epochBlock "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData po ON ph.id = po.poolId AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.poolId = ph.id)) "
              + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHashId AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHashId = ph.id)"
              + "LEFT JOIN AggregatePoolInfo api on api.poolId = ph.id "
              + "WHERE ph.id NOT IN :exceptPoolIds "
              + "AND ( :param is null OR ph.view = :param OR ph.hashRaw = :param "
              + "OR LOWER(po.poolName) LIKE CONCAT('%', :param, '%') OR LOWER(po.tickerName) LIKE CONCAT('%', :param, '%'))"
              + "AND ( :minPledge <= coalesce(pu.pledge,0) and :maxPledge >= coalesce(pu.pledge,0) )"
              + "AND ( :minVotingPower <= coalesce(api.votingPower,0) and :maxVotingPower >= coalesce(api.votingPower,0)) "
              + "AND ( :minGovParticipationRate <= coalesce(api.governanceParticipationRate,0) and :maxGovParticipationRate >= coalesce(api.governanceParticipationRate,0)) "
              + "AND ( :minBlockLifeTime <= coalesce(api.blockLifeTime,0) and :maxBlockLifeTime >= coalesce(api.blockLifeTime,0))")
  Page<PoolListProjection> findAllWithoutUsingKoi0s(
      @Param("param") String param,
      @Param("exceptPoolIds") Collection<Long> exceptPoolIds,
      @Param("minPledge") BigInteger minPledge,
      @Param("maxPledge") BigInteger maxPledge,
      @Param("minVotingPower") Double minVotingPower,
      @Param("maxVotingPower") Double maxVotingPower,
      @Param("minGovParticipationRate") Double minGovParticipationRate,
      @Param("maxGovParticipationRate") Double maxGovParticipationRate,
      @Param("minBlockLifeTime") Integer minBlockLifeTime,
      @Param("maxBlockLifeTime") Integer maxBlockLifeTime,
      Pageable pageable);

  @Query(
      value =
          "SELECT ph.id AS poolId, ph.view AS poolView, po.poolName AS poolName, pu.pledge AS pledge, "
              + "po.tickerName as tickerName, LENGTH(po.poolName) as poolNameLength, "
              + "COALESCE(pi.activeStake, -1) AS poolSize, COALESCE(pi.liveSaturation, -1) AS saturation, "
              + "COALESCE(api.governanceParticipationRate, -1) as governanceParticipationRate, COALESCE(api.votingPower, -1) as votingPower,"
              + "COALESCE(api.blockLifeTime, 0) as lifetimeBlock, COALESCE(api.blockInEpoch, 0) as epochBlock "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData po ON ph.id = po.poolId AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.poolId = ph.id)) "
              + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHashId AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHashId = ph.id) "
              + "LEFT JOIN PoolInfo pi ON ph.id = pi.poolId AND pi.fetchedAtEpoch = :epochNo "
              + "LEFT JOIN AggregatePoolInfo api on api.poolId = ph.id "
              + "WHERE ph.id NOT IN :exceptPoolIds "
              + "AND ( :param is null OR  ph.view = :param OR ph.hashRaw = :param "
              + "OR LOWER(po.poolName) LIKE CONCAT('%', :param, '%')  OR LOWER(po.tickerName) LIKE CONCAT('%', :param, '%'))"
              + "AND ( :minPoolSize <= coalesce(pi.activeStake, 0) and :maxPoolSize >= coalesce(pi.activeStake,0) )"
              + "AND ( :minPledge <= coalesce(pu.pledge,0) and :maxPledge >= coalesce(pu.pledge,0) )"
              + "AND ( :minSaturation <= coalesce(pi.liveSaturation,0) and :maxSaturation >= coalesce(pi.liveSaturation,0) ) "
              + "AND ( :minVotingPower <= coalesce(api.votingPower,0) and :maxVotingPower >= coalesce(api.votingPower,0) ) "
              + "AND ( :minGovParticipationRate <= coalesce(api.governanceParticipationRate,0) and :maxGovParticipationRate >= coalesce(api.governanceParticipationRate,0) ) "
              + "AND ( :minBlockLifeTime <= coalesce(api.blockLifeTime,0) and :maxBlockLifeTime >= coalesce(api.blockLifeTime,0) ) ")
  Page<PoolListProjection> findAllWithUsingKoiOs(
      @Param("param") String param,
      @Param("exceptPoolIds") Collection<Long> exceptPoolIds,
      @Param("epochNo") Integer epochNo,
      @Param("minPoolSize") BigInteger minPoolSize,
      @Param("maxPoolSize") BigInteger maxPoolSize,
      @Param("minPledge") BigInteger minPledge,
      @Param("maxPledge") BigInteger maxPledge,
      @Param("minSaturation") Double minSaturation,
      @Param("maxSaturation") Double maxSaturation,
      @Param("minVotingPower") Double minVotingPower,
      @Param("maxVotingPower") Double maxVotingPower,
      @Param("minGovParticipationRate") Double minGovParticipationRate,
      @Param("maxGovParticipationRate") Double maxGovParticipationRate,
      @Param("minBlockLifeTime") Integer minBlockLifeTime,
      @Param("maxBlockLifeTime") Integer maxBlockLifeTime,
      Pageable pageable);

  @Query(value = "SELECT ph.id FROM PoolHash ph " + "WHERE ph.view IN :poolViews ")
  Set<Long> getListPoolIdIn(@Param("poolViews") Set<String> poolViews);

  @Query(
      value =
          "SELECT ph FROM PoolHash ph "
              + "WHERE (ph.view = :poolViewOrHash "
              + "OR ph.hashRaw = :poolViewOrHash)")
  Optional<PoolHash> findByViewOrHashRaw(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(
      value =
          "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, po.poolName AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, "
              + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, sa.view AS rewardAddress, "
              + "po.json as json, po.iconUrl AS iconUrl, po.logoUrl AS logoUrl, ph.view AS poolView "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id  = ph.id)) "
              + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id  = ph.id) "
              + "LEFT JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
              + "LEFT JOIN EpochParam ep ON ep.epochNo = :epochNo "
              + "WHERE (ph.view = :poolViewOrHash "
              + "OR ph.hashRaw = :poolViewOrHash)")
  PoolDetailUpdateProjection getDataForPoolDetailNoReward(
      @Param("poolViewOrHash") String poolViewOrHash, @Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT ph.id AS poolId, ph.hashRaw AS hashRaw, po.poolName AS poolName, po.tickerName AS tickerName, pu.pledge AS pledge, pu.margin AS margin, "
              + "pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.reserves AS reserves, sa.view AS rewardAddress, "
              + "po.json as json, po.iconUrl AS iconUrl, po.logoUrl AS logoUrl, ph.view AS poolView "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData po ON ph.id = po.pool.id AND (po.id is NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.pool.id  = ph.id)) "
              + "LEFT JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHash.id  = ph.id) "
              + "LEFT JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
              + "LEFT JOIN EpochParam ep ON ep.epochNo = :epochNo "
              + "LEFT JOIN AdaPots ap ON ap.epochNo = :epochNo "
              + "WHERE (ph.view = :poolViewOrHash "
              + "OR ph.hashRaw = :poolViewOrHash)")
  PoolDetailUpdateProjection getDataForPoolDetail(
      @Param("poolViewOrHash") String poolViewOrHash, @Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, tx.deposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
              + "FROM PoolUpdate pu "
              + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
              + "JOIN Block bk ON tx.block.id  = bk.id "
              + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
              + "WHERE pu.id = :id")
  PoolRegistrationProjection getPoolRegistration(@Param("id") Long id);

  @Query(
      value =
          "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView, pod.iconUrl as icon "
              + "FROM PoolHash ph "
              + "LEFT JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = (SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id ) "
              + "WHERE (ph.view = :poolViewOrHash "
              + "OR ph.hashRaw = :poolViewOrHash)")
  PoolInfoProjection getPoolInfo(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(
      value =
          "SELECT pu.id AS poolUpdateId, pu.pledge AS pledge, pu.margin AS margin, pu.vrfKeyHash AS vrfKey, pu.fixedCost AS cost, tx.hash AS txHash, bk.time AS time, ep.poolDeposit AS deposit, tx.fee AS fee, sa.view AS rewardAccount "
              + "FROM PoolUpdate pu "
              + "JOIN Tx tx ON pu.registeredTx.id = tx.id "
              + "JOIN Block bk ON tx.block.id  = bk.id "
              + "JOIN EpochParam ep ON ep.epochNo = bk.epochNo "
              + "JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
              + "WHERE pu.id IN :poolCertificateIds ")
  Page<PoolRegistrationProjection> getPoolRegistrationByPool(
      @Param("poolCertificateIds") Set<Long> poolCertificateIds, Pageable pageable);

  @Query(
      value =
          "SELECT ph.id AS id, pod.poolName AS poolName, ph.hashRaw AS poolId, ph.view AS poolView, pod.iconUrl as icon "
              + "FROM PoolHash ph "
              + "INNER JOIN PoolOfflineData pod ON ph.id  = pod.pool.id AND pod.id = "
              + "(SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.pool.id ) "
              + "WHERE LOWER(pod.poolName) LIKE CONCAT('%', :query, '%') OR "
              + "LOWER(pod.tickerName) LIKE CONCAT('%', :query, '%') ")
  List<PoolInfoProjection> findByPoolNameLike(@Param("query") String query, Pageable pageable);

  @Query(
      value =
          "select min(d.slotNo) from PoolHash ph"
              + " join Delegation d on d.poolHash.id = ph.id"
              + " where ph.hashRaw =:poolHash")
  Long getSlotNoWhenFirstDelegationByPoolHash(@Param("poolHash") String poolHash);

  @Query(
      value =
          """
    select min(d.slotNo) as createdAt, ph.hashRaw as poolHash, ph.id as poolId from PoolHash ph
    join Delegation d on d.poolHash.id = ph.id
    group by ph.hashRaw, ph.id
    having min(d.slotNo) >= :slot
    order by ph.id desc
""")
  List<PoolOverviewProjection> getSlotCreatedAtGroupByPoolHash(@Param("slot") Long slot);

  @Query(value = "select ph.hashRaw from PoolHash ph where ph.view = :view")
  Optional<String> getHashRawByView(@Param("view") String view);

  @Query(
      value =
          "SELECT pod.poolName AS poolName"
              + " FROM PoolHash ph"
              + " INNER JOIN PoolOfflineData pod ON ph.id  = pod.poolId AND pod.id ="
              + " (SELECT max(pod2.id) FROM PoolOfflineData pod2 WHERE ph.id = pod2.poolId)"
              + " WHERE  ph.hashRaw = :poolViewOrHash or ph.view = :poolViewOrHash")
  Optional<String> getPoolNameByPoolHashOrPoolView(@Param("poolViewOrHash") String poolViewOrHash);

  @Query(
      value =
          " SELECT min(pi.activeStake) as minPoolSize, max(pi.activeStake) as maxPoolSize,"
              + " min(pi.liveSaturation) as minSaturation, max(pi.liveSaturation) as maxSaturation,"
              + " min(pu.pledge) as minPledge, max(pu.pledge) as maxPledge,"
              + " min(api.votingPower) as minVotingPower, max(api.votingPower) as maxVotingPower,"
              + " min (api.governanceParticipationRate) as minGovParticipationRate, max(api.governanceParticipationRate) as maxGovParticipationRate,"
              + " min (api.blockLifeTime) as minLifetimeBlock, max(api.blockLifeTime) as maxLifetimeBlock"
              + " FROM PoolHash ph"
              + " left join PoolOfflineData po ON ph.id = po.poolId AND (po.id IS NULL OR po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.poolId = ph.id))"
              + " left join PoolUpdate pu ON ph.id = pu.poolHashId AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHashId = ph.id)"
              + " LEFT JOIN PoolInfo pi ON ph.id = pi.poolId AND pi.fetchedAtEpoch = :epochNo "
              + " left join AggregatePoolInfo api on api.poolId = ph.id ")
  PoolRangeProjection getPoolRangeWithUsingKoi0s(@Param("epochNo") Integer epochNo);

  @Query(
      value =
          " SELECT min(pu.pledge) as minPledge, max(pu.pledge) as maxPledge,"
              + " min(api.votingPower) as minVotingPower, max(api.votingPower) as maxVotingPower,"
              + " min (api.governanceParticipationRate) as minGovParticipationRate, max(api.governanceParticipationRate) as maxGovParticipationRate,"
              + " min (api.blockLifeTime) as minLifetimeBlock, max(api.blockLifeTime) as maxLifetimeBlock"
              + " FROM PoolHash ph"
              + " left join PoolUpdate pu ON ph.id = pu.poolHashId AND pu.id = (SELECT max(pu2.id) FROM PoolUpdate pu2 WHERE pu2.poolHashId = ph.id)"
              + " left join AggregatePoolInfo api on api.poolId = ph.id ")
  PoolRangeProjection getPoolRangeWithoutUsingKoi0s();
}
