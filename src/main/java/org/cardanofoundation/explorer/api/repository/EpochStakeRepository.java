package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochStakeProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolReportProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.consumercommon.entity.EpochStake;
import java.math.BigInteger;
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
public interface EpochStakeRepository extends JpaRepository<EpochStake, Long> {

  @Query(value = "SELECT es.epochNo AS epochNo, sum(es.amount) AS totalStake FROM EpochStake es "
      + "WHERE es.epochNo IN :epochNo AND es.pool.id = :poolId "
      + "GROUP BY es.epochNo")
  List<EpochStakeProjection> totalStakeByEpochNoAndPool(@Param("epochNo") Set<Integer> epochNo,
      @Param("poolId") Long poolId);

  @Query(value = "SELECT sum(es.amount) FROM EpochStake es WHERE es.epochNo = :epochNo")
  Optional<BigInteger> totalStakeAllPoolByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(value = "SELECT sum(es.amount) FROM EpochStake es WHERE es.pool.id = :poolId "
          + "AND es.epochNo = (SELECT max(e.no) FROM Epoch e)")
  BigInteger activeStakeByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT es.epochNo AS chartKey, sum(es.amount) AS chartValue FROM EpochStake es "
          + "WHERE es.pool.id = :poolId "
          + "GROUP BY es.epochNo "
          + "ORDER BY es.epochNo ASC ")
  List<EpochChartProjection> getDataForEpochChart(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT es.addr.id AS address, sum(es.amount) AS totalStake FROM EpochStake es WHERE es.addr.id IN :stakeAddressIds AND es.pool.id = :poolId "
          + "GROUP BY es.addr.id")
  List<StakeAddressProjection> totalStakeByAddressAndPool(
      @Param("stakeAddressIds") Set<Long> stakeAddressIds,
      @Param("poolId") Long poolId);

  @Query(value =
      "SELECT es.epochNo as epochNo, pu.fixedCost as fee, sum(es.amount) as size "
          + "FROM EpochStake es "
          + "LEFT JOIN PoolUpdate pu ON pu.poolHashId = es.pool.id "
          + "where es.pool.view = :poolView "
          + "and es.epochNo between :epochBegin and :epochEnd "
          + "group by es.epochNo, pu.fixedCost")
  Page<PoolReportProjection> getEpochSizeByPoolReport(@Param("poolView") String poolView,
                                                      @Param("epochBegin") int epochBegin,
                                                      @Param("epochEnd") int epochEnd,
                                                      Pageable pageable);

  @Query(value =
      "SELECT es.epochNo as epochNo, pu.fixedCost as fee, sum(es.amount) as size "
          + "FROM EpochStake es "
          + "LEFT JOIN PoolUpdate pu ON pu.poolHashId = es.pool.id "
          + "where es.pool.view = :poolView "
          + "and es.epochNo between :epochBegin and :epochEnd "
          + "group by es.epochNo, pu.fixedCost")
  List<PoolReportProjection> getEpochSizeByPoolReport(@Param("poolView") String poolView,
                                                      @Param("epochBegin") int epochBegin,
                                                      @Param("epochEnd") int epochEnd);
}
