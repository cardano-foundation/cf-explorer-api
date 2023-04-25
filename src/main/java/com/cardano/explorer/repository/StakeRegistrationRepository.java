package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardano.common.entity.StakeRegistration;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface StakeRegistrationRepository extends JpaRepository<StakeRegistration, Long> {

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.blockNo AS blockId, bk.epochNo AS epochNo, "
          + "bk.slotNo as slotNo, bk.epochSlotNo as epochSlotNo, sr.addr.view as stakeKey "
          + "FROM StakeRegistration sr "
          + "JOIN Tx tx ON tx.id = sr.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "ORDER BY tx.blockId DESC, tx.blockIndex DESC",
      countQuery = "SELECT count(id) FROM StakeRegistration")
  Page<TrxBlockEpochStake> getDataForStakeRegistration(Pageable pageable);

  @Query("SELECT max(stakeRegis.tx.id) "
      + " FROM StakeRegistration stakeRegis"
      + " WHERE stakeRegis.addr = :stake")
  Optional<Long> findMaxTxIdByStake(StakeAddress stake);

  @Query(value = "SELECT tx.hash as txHash, b.time as time,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " 'Registered' AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
      + " FROM StakeRegistration sr"
      + " JOIN Tx tx ON tx.id = sr.tx.id"
      + " JOIN Block b ON b.id = tx.blockId"
      + " WHERE sr.addr = :stakeKey"
      + " ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<StakeHistoryProjection> getStakeRegistrationsByAddress(StakeAddress stakeKey);

  @Query(value = "SELECT sr.tx.id"
      + " FROM StakeRegistration sr"
      + " WHERE sr.addr = :stakeKey AND sr.tx.id IN :txIds")
  List<Long> getStakeRegistrationsByAddressAndTxIn(StakeAddress stakeKey, Collection<Long> txIds);

  @Query(value = "SELECT tx.hash as txHash, b.time as time,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " 'Registered' AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
      + " FROM StakeRegistration sr"
      + " JOIN Tx tx ON tx.id = sr.tx.id"
      + " JOIN Block b ON b.id = tx.blockId"
      + " WHERE sr.addr = :stakeKey"
      + " AND (b.time >= :fromTime ) "
      + " AND (b.time <= :toTime)"
      + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeHistoryProjection> getStakeRegistrationsByAddress(StakeAddress stakeKey, String txHash,
      Timestamp fromTime, Timestamp toTime, Pageable pageable);
}
