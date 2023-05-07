package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(String aLong);


  @Query(value = "SELECT DISTINCT tx.hash as txHash, b.time as time,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " (CASE WHEN sr.id IS NULL THEN 'De Registered' ELSE 'Registered' END) AS action, tx.blockIndex"
      + " FROM TxOut txo"
      + " JOIN Tx tx ON tx.id = txo.tx.id "
      + " JOIN Block b ON b.id = tx.blockId  "
      + " LEFT JOIN StakeRegistration sr ON  sr.tx.id  =  txo.tx.id AND sr.addr.id = txo.stakeAddress.id"
      + " LEFT JOIN StakeDeregistration sd ON sd.tx.id  = txo.tx.id AND sd.addr.id = txo.stakeAddress.id"
      + " WHERE txo.stakeAddress.id = (SELECT sa.id FROM StakeAddress sa WHERE sa.view = :stakeKey) AND"
      + " (sr.id IS NOT NULL OR sd.id IS NOT NULL) AND"
      + " tx.id IS NOT NULL"
      + " ORDER BY b.blockNo DESC, tx.blockIndex DESC",
      countQuery = "SELECT COUNT(DISTINCT tx.id)"
      + " FROM TxOut txo"
      + " JOIN Tx tx ON tx.id = txo.tx.id "
      + " JOIN Block b ON b.id = tx.blockId  "
      + " LEFT JOIN StakeRegistration sr ON  sr.tx.id  =  txo.tx.id "
      + " LEFT JOIN StakeDeregistration sd ON sd.tx.id  = txo.tx.id"
      + " WHERE txo.stakeAddress.id = (SELECT sa.id FROM StakeAddress sa WHERE sa.view = :stakeKey) AND"
      + " (sr.id IS NOT NULL OR sd.id IS NOT NULL) AND"
      + " tx.id IS NOT NULL")
  Page<StakeHistoryProjection> getStakeHistory(String stakeKey, Pageable pageable);

  @Query(value = "SELECT sa.view as stakeAddress, sum(addr.balance) as totalStake"
      + " FROM StakeAddress sa"
      + " LEFT JOIN Address addr ON addr.stakeAddress = sa"
      + " WHERE EXISTS (SELECT d FROM Delegation d WHERE d.address = sa)"
      + " GROUP BY sa.id"
      + " HAVING sum(addr.balance) IS NOT NULL"
      + " ORDER BY totalStake DESC")
  List<StakeAddressProjection> findStakeAddressOrderByBalance(Pageable pageable);

  @Query(value = "SELECT ph.view FROM StakeAddress sa "
      + "JOIN PoolOwner po ON sa.id  = po.stakeAddress.id "
      + "JOIN PoolUpdate pu ON po.poolUpdate.id  = pu.id "
      + "JOIN PoolHash ph ON pu.poolHash.id = ph.id "
      + "WHERE sa.view = :stakeKey "
      + "GROUP BY ph.view ")
  Page<String> getPoolViewByStakeKey(@Param("stakeKey") String stakeKey, Pageable pageable);
}
