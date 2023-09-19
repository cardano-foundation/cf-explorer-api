package org.cardanofoundation.explorer.api.repository;

import java.math.BigInteger;
import java.util.List;
import java.util.Set;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoKoiosProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolInfo;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolInfoRepository extends JpaRepository<PoolInfo, Long> {

  @Query(value = "SELECT SUM(pi.liveStake) FROM PoolInfo pi WHERE pi.fetchedAtEpoch = :epochNo")
  BigInteger getTotalLiveStake(@Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT pi.pool.view AS view, pi.activeStake AS activeStake, pi.liveSaturation AS saturation "
          + "FROM PoolInfo pi "
          + "WHERE pi.pool.view IN :poolIds AND pi.fetchedAtEpoch = :epochNo")
  List<PoolInfoKoiosProjection> getPoolInfoKoios(@Param("poolIds") Set<String> poolIds,
      @Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT pi.pool.view AS view, pi.activeStake AS activeStake "
          + "FROM PoolInfo pi "
          + "WHERE pi.fetchedAtEpoch = :epochNo AND pi.activeStake IS NOT NULL "
          + "ORDER BY pi.activeStake DESC")
  List<PoolInfoKoiosProjection> getTopPoolInfoKoios(@Param("epochNo") Integer epochNo,
      Pageable pageable);

  @Query(value = "SELECT pi.activeStake FROM PoolInfo pi "
      + "WHERE pi.pool.view = :poolId AND pi.fetchedAtEpoch = :epochNo")
  BigInteger getActiveStakeByPoolAndEpoch(@Param("poolId") String poolId,
      @Param("epochNo") Integer epochNo);

  @Query(value = "SELECT SUM(pi.activeStake) FROM PoolInfo pi WHERE pi.fetchedAtEpoch = :epochNo")
  BigInteger getTotalActiveStake(@Param("epochNo") Integer epochNo);
}
