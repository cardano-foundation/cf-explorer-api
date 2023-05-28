package org.cardanofoundation.explorer.api.repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.pool.projection.RewardEpochProjection;
import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;
import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findFirstByNo(@Param("no") Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();

  @Query(value = "SELECT no as no, maxSlot as maxSlot , startTime as startTime "
      + "FROM Epoch "
      + "WHERE no  = (SELECT MAX(no) FROM  Epoch)")
  Optional<EpochSummaryProjection> findCurrentEpochSummary();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no = (SELECT max(no) FROM Epoch)")
  Optional<Epoch> findByCurrentEpochNo();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no IN :epochNo")
  List<Epoch> findFeeByEpochNo(@Param("epochNo") Set<Integer> epochNo);

  @Query(value =
      "SELECT e.no AS epochNo, ep.monetaryExpandRate AS expansionRate, ep.treasuryGrowthRate AS treasuryRate, e.blkCount AS blkCount, "
          + "ep.optimalPoolCount AS paramK, ep.influence AS influence, e.fees AS feePerEpoch, ap.utxo AS utxo, ep.maxBlockSize AS maxBlockSize "
          + "FROM EpochParam ep "
          + "JOIN AdaPots ap ON ap.epochNo = ep.epochNo "
          + "JOIN Epoch e ON e.no = ep.epochNo "
          + "WHERE e.no IN :epochNo")
  List<RewardEpochProjection> findParamRewardByEpoch(@Param("epochNo") Set<Integer> epochNo);

  @Query(value = "SELECT  e.no AS epochNo , e.startTime AS startTime , e.endTime AS endTime "
      + "FROM Epoch e "
      + "WHERE e.no BETWEEN :min AND :max")
  List<EpochTimeProjection> findEpochTime(@Param("min") Integer min, @Param("max") Integer max);
}