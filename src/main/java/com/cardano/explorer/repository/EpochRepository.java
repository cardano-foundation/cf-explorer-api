package com.cardano.explorer.repository;

import com.cardano.explorer.projection.EpochSummaryProjection;
import com.sotatek.cardano.common.entity.Epoch;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findByNo(Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();

  @Query(value = "SELECT no as no, maxSlot as maxSlot , startTime as startTime "
      + "FROM Epoch "
      + "WHERE no  = (SELECT MAX(no) FROM  Epoch)")
  Optional<EpochSummaryProjection> findCurrentEpochSummary();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no = (SELECT max(no) FROM Epoch)")
  Optional<Epoch> findByCurrentEpochNo();
}