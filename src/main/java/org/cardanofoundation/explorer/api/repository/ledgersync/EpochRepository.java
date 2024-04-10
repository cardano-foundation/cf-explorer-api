package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;
import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findFirstByNo(@Param("no") Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();

  @Query(
      value =
          "SELECT no as no, blkCount as blkCount, maxSlot as maxSlot , startTime as startTime"
              + ",endTime as endTime "
              + "FROM Epoch "
              + "WHERE no  = (SELECT MAX(epoch.no) FROM Epoch epoch)")
  Optional<EpochSummaryProjection> findCurrentEpochSummary();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no = (SELECT max(epoch.no) FROM Epoch epoch)")
  Optional<Epoch> findByCurrentEpochNo();

  @Query(
      value =
          "SELECT new org.cardanofoundation.explorer.api.projection.EpochTimeProjection( "
              + "e.no, e.startTime, e.endTime) "
              + "FROM Epoch e "
              + "WHERE e.no BETWEEN :min AND :max")
  List<EpochTimeProjection> findEpochTime(@Param("min") Integer min, @Param("max") Integer max);
}
