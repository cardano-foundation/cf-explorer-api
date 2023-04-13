package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.RewardEpochProjection;
import com.cardano.explorer.projection.EpochSummaryProjection;
import com.sotatek.cardano.common.entity.Epoch;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findFirstByNo(Integer no);

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

  @Query(value = "SELECT  COUNT(DISTINCT "
      + "(CASE WHEN addr.stakeAddress.id IS NULL THEN addr.address "
      + "WHEN addr.stakeAddress.id IS NULL THEN CAST(addr.stakeAddress.id AS string) END))  "
      + "FROM Block  b "
      + "JOIN Tx tx ON tx.blockId  = b.id "
      + "JOIN AddressTxBalance  atb ON atb.tx.id = tx.id "
      + "JOIN Address addr ON addr.id = atb.address.id "
      + "WHERE b.epochNo  = :epochNo ")
  Integer getTotalAccountsAtEpoch(Integer epochNo);

}