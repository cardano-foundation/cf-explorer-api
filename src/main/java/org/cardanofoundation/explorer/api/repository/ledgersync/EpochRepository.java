package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;
import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;
import org.cardanofoundation.explorer.api.projection.UniqueAddressProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findFirstByNo(@Param("no") Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();

  @Query(value = "SELECT no as no, blkCount as blkCount, maxSlot as maxSlot , startTime as startTime"
      + ",endTime as endTime "
      + "FROM Epoch "
      + "WHERE no  = (SELECT MAX(epoch.no) FROM Epoch epoch)")
  Optional<EpochSummaryProjection> findCurrentEpochSummary();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no = (SELECT max(epoch.no) FROM Epoch epoch)")
  Optional<Epoch> findByCurrentEpochNo();

  @Query(value = "SELECT ep FROM Epoch ep WHERE ep.no IN :epochNo")
  List<Epoch> findFeeByEpochNo(@Param("epochNo") Set<Integer> epochNo);

  @Query(value = "SELECT  DISTINCT "
      + "(CASE WHEN addr.stakeAddress.id IS NULL THEN addr.address   "
      + "WHEN addr.stakeAddress.id IS NOT NULL THEN CAST(addr.stakeAddressId AS string) END) AS address,"
      + "MAX(tx.id) as id "
      + "FROM Block  b "
      + "JOIN Tx tx ON tx.blockId  = b.id "
      + "JOIN AddressTxBalance  atb ON atb.tx.id = tx.id "
      + "JOIN Address addr ON addr.id = atb.address.id "
      + "WHERE tx.id > :txId AND "
      + "b.epochNo  = :epochNo "
      + "GROUP BY addr.stakeAddressId, addr.address")
  List<UniqueAddressProjection> getTotalAccountsAtEpoch(@Param("epochNo") Integer epochNo, @Param("txId") Long txId);


  @Query(value = "SELECT  e.no AS epochNo , e.startTime AS startTime , e.endTime AS endTime "
      + "FROM Epoch e "
      + "WHERE e.no BETWEEN :min AND :max")
  List<EpochTimeProjection> findEpochTime(@Param("min") Integer min, @Param("max") Integer max);
}