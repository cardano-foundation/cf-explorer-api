package com.cardano.explorer.repository;

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
}