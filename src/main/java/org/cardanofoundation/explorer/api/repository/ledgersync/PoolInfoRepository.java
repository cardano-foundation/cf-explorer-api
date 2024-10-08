package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoKoiosProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolInfo;

@Repository
public interface PoolInfoRepository extends JpaRepository<PoolInfo, Long> {

  @Query(value = "SELECT SUM(pi.liveStake) FROM PoolInfo pi WHERE pi.fetchedAtEpoch = :epochNo")
  BigInteger getTotalLiveStake(@Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT pi.pool.view AS view, pi.activeStake AS activeStake, pi.liveSaturation AS saturation "
              + "FROM PoolInfo pi "
              + "WHERE pi.pool.view IN :poolIds AND pi.fetchedAtEpoch = :epochNo")
  List<PoolInfoKoiosProjection> getPoolInfoKoios(
      @Param("poolIds") Set<String> poolIds, @Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT pi.activeStake FROM PoolInfo pi "
              + "WHERE (pi.pool.view = :poolId OR pi.pool.hashRaw = :poolId) AND pi.fetchedAtEpoch = :epochNo")
  BigInteger getActiveStakeByPoolAndEpoch(
      @Param("poolId") String poolId, @Param("epochNo") Integer epochNo);

  @Query(value = "SELECT SUM(pi.activeStake) FROM PoolInfo pi WHERE pi.fetchedAtEpoch = :epochNo")
  BigInteger getTotalActiveStake(@Param("epochNo") Integer epochNo);

  @Query(
      """
    SELECT SUM(pi.activeStake) FROM PoolInfo pi WHERE pi.pool.hashRaw IN :poolHashes AND pi.fetchedAtEpoch = :epochNo
    """)
  BigInteger getTotalActiveStakeByPoolIdIn(
      @Param("poolHashes") Collection<String> poolHashes, @Param("epochNo") Integer epochNo);
}
