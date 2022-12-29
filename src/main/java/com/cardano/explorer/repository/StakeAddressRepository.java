package com.cardano.explorer.repository;

import com.cardano.explorer.projection.StakeAddressProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.sotatek.cardano.common.entity.StakeAddress;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(String aLong);


  @Query(value = "SELECT DISTINCT tx.hash as txHash, b.time as time,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " (CASE WHEN sr.id IS NULL THEN 'De Registered' ELSE 'Registered' END) AS action"
      + " FROM TxOut txo"
      + " JOIN Tx tx ON tx.id = txo.tx.id "
      + " JOIN Block b ON b.id = tx.blockId  "
      + " LEFT JOIN StakeRegistration sr ON  sr.tx.id  =  txo.tx.id AND sr.addr.id = txo.stakeAddress.id"
      + " LEFT JOIN StakeDeregistration sd ON sd.tx.id  = txo.tx.id AND sd.addr.id = txo.stakeAddress.id"
      + " WHERE txo.stakeAddress.id = (SELECT sa.id FROM StakeAddress sa WHERE sa.view = :stakeKey) AND"
      + " (sr.id IS NOT NULL OR sd.id IS NOT NULL) AND"
      + " tx.id IS NOT NULL"
      + " ORDER BY b.time DESC",
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

  @Query("SELECT sa.view as stakeAddress, sa.balance as totalStake"
      + " FROM StakeAddress sa"
      + " WHERE EXISTS (SELECT d FROM Delegation d WHERE d.address = sa)"
      + " ORDER BY sa.balance DESC")
  Page<StakeAddressProjection> findStakeAddressOrderByBalance(Pageable pageable);

}
