package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.EpochChartProjection;
import com.cardano.explorer.projection.PoolDelegationSizeProjection;
import com.sotatek.cardano.common.entity.EpochStake;
import java.math.BigDecimal;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface EpochStakeRepository extends JpaRepository<EpochStake, Long> {

  @Query(value = "SELECT sum(es.amount) FROM EpochStake es WHERE es.epochNo = :epochNo AND es.pool.id = :poolId")
  BigDecimal totalStakeByEpochNoAndPool(@Param("epochNo") Integer epochNo,
      @Param("poolId") Long poolId);

  @Query(value = "SELECT sum(es.amount) FROM EpochStake es WHERE es.epochNo = :epochNo")
  BigDecimal totalStakeAllPoolByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(value =
      "SELECT es.epochNo AS chartKey, sum(es.amount) AS chartValue FROM EpochStake es "
          + "WHERE es.pool.id = :poolId "
          + "GROUP BY es.epochNo "
          + "ORDER BY es.epochNo ASC ")
  List<EpochChartProjection> getDataForEpochChart(@Param("poolId") Long poolId);

  @Query(value = "SELECT sum(es.amount) FROM EpochStake es WHERE es.addr.id = :stakeAddressId AND es.pool.id = :poolId")
  BigDecimal totalStakeByAddressAndPool(@Param("stakeAddressId") Long stakeAddressId,
      @Param("poolId") Long poolId);

  @Query("SELECT SUM(es.amount) AS poolSize, es.pool.id as poolId "
      + "FROM EpochStake es "
      + "GROUP BY es.pool.id "
      + "ORDER BY poolSize DESC ")
  List<PoolDelegationSizeProjection> findPoolsDelegationSize(Pageable pageable);
}
