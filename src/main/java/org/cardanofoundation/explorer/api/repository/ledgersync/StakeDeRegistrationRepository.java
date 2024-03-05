package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeDeregistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistration_;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;

@Repository
public interface StakeDeRegistrationRepository extends JpaRepository<StakeDeregistration, Long> {

  @Query(
      "SELECT max(stakeDeregis.tx.id) "
          + " FROM StakeDeregistration stakeDeregis"
          + " WHERE stakeDeregis.addr = :stake")
  Optional<Long> findMaxTxIdByStake(@Param("stake") StakeAddress stake);

  @Query(
      value =
          "SELECT tx.hash as txHash, b.time as time,"
              + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo, b.slotNo as slotNo,"
              + " 'De Registered' AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
              + " FROM StakeDeregistration sd"
              + " JOIN Tx tx ON tx.id = sd.tx.id"
              + " JOIN Block b ON b.id = tx.blockId"
              + " WHERE sd.addr = :stakeKey"
              + " ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<StakeHistoryProjection> getStakeDeRegistrationsByAddress(
      @Param("stakeKey") StakeAddress stakeKey);

  @Query(
      value =
          "SELECT sd.tx.id"
              + " FROM StakeDeregistration sd"
              + " WHERE sd.addr = :stakeKey AND sd.tx.id IN :txIds")
  List<Long> getStakeDeRegistrationsByAddressAndTxIn(
      @Param("stakeKey") StakeAddress stakeKey, @Param("txIds") Collection<Long> txIds);

  @Query(
      value =
          "SELECT tx.hash as txHash, b.time as time,"
              + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
              + " 'De Registered' AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
              + " FROM StakeDeregistration dr"
              + " JOIN Tx tx ON tx.id = dr.tx.id"
              + " JOIN Block b ON b.id = tx.blockId"
              + " WHERE dr.addr = :stakeKey"
              + " AND (b.time >= :fromTime ) "
              + " AND (b.time <= :toTime)"
              + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeHistoryProjection> getStakeDeRegistrationsByAddress(
      @Param("stakeKey") StakeAddress stakeKey,
      @Param("txHash") String txHash,
      @Param("fromTime") Timestamp fromTime,
      @Param("toTime") Timestamp toTime,
      Pageable pageable);

  @Query(
      value =
          "SELECT tx.hash as txHash, b.time as time, b.epochNo as epochNo,"
              + " tx.fee as fee, tx.deposit as deposit"
              + " FROM StakeDeregistration sd"
              + " JOIN Tx tx ON tx.id = sd.tx.id"
              + " JOIN Block b ON b.id = tx.blockId"
              + " JOIN StakeAddress sa ON sa.id = sd.addr.id"
              + " WHERE sa.view = :stakeKey"
              + " AND tx.hash = :txHash")
  Optional<StakeHistoryProjection> findByAddressAndTx(
      @Param("stakeKey") String stakeKey, @Param("txHash") String txHash);

  @EntityGraph(attributePaths = {StakeRegistration_.ADDR})
  List<StakeDeregistration> findByTx(@Param("tx") Tx tx);

  Boolean existsByAddr(@Param("stakeAddress") StakeAddress stakeAddress);
}
