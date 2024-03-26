package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.bloxbean.cardano.client.transaction.spec.cert.CertificateType;

import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistration;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeRegistrationId;

@Repository
public interface YaciStakeRegistrationRepository
    extends JpaRepository<StakeRegistration, StakeRegistrationId> {

  @Query("SELECT sr FROM StakeRegistration sr WHERE sr.type = :type")
  Page<StakeRegistration> findAllStake(@Param("type") CertificateType type, Pageable pageable);

  @Query(
      "SELECT max(tx.id) "
          + " FROM StakeRegistration stakeRegis"
          + " JOIN Tx tx ON tx.hash = stakeRegis.txHash"
          + " WHERE stakeRegis.address = :stakeAddress AND stakeRegis.type = :type")
  Optional<Long> findMaxTxIdByStake(
      @Param("stakeAddress") String stakeAddress, @Param("type") CertificateType type);

  @Query(
      "SELECT tx.hash as txHash, b.time as time, b.slotNo as slotNo,"
          + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
          + " sr.type AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
          + " FROM StakeRegistration sr"
          + " JOIN Tx tx ON tx.hash = sr.txHash"
          + " JOIN Block b ON b.hash = sr.blockHash"
          + " WHERE sr.address = :stakeAddress AND sr.type = :type"
          + " ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<StakeHistoryProjection> getStakeRegistrationsByAddress(
      @Param("stakeAddress") String stakeAddress, @Param("type") CertificateType type);

  @Query(
      "SELECT tx.id"
          + " FROM StakeRegistration sr"
          + " JOIN Tx tx ON tx.hash = sr.txHash"
          + " WHERE sr.address = :stakeAddress AND sr.type = :type AND tx.id IN :txIds")
  List<Long> getStakeRegistrationsByAddressAndTxIn(
      @Param("stakeAddress") String stakeAddress,
      @Param("txIds") List<Long> txIds,
      @Param("type") CertificateType type);

  @Query(
      "SELECT tx.hash as txHash, b.time as time, b.slotNo as slotNo,"
          + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
          + " sr.type AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
          + " FROM StakeRegistration sr"
          + " JOIN Tx tx ON tx.hash = sr.txHash"
          + " JOIN Block b ON b.hash = sr.blockHash"
          + " WHERE sr.address = :stakeAddress"
          + " AND (b.time >= :fromTime)"
          + " AND (b.time <= :toTime)"
          + " AND sr.type = :type"
          + " AND (:txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeHistoryProjection> getStakeRegistrationsByAddress(
      @Param("stakeAddress") String stakeAddress,
      @Param("txHash") String txHash,
      @Param("fromTime") Timestamp fromTime,
      @Param("toTime") Timestamp toTime,
      @Param("type") CertificateType type,
      Pageable pageable);

  @Query(
      "SELECT tx.hash as txHash, b.time as time, b.slotNo as slotNo,"
          + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
          + " sr.type AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
          + " FROM StakeRegistration sr"
          + " JOIN Tx tx ON tx.hash = sr.txHash"
          + " JOIN Block b ON b.hash = sr.blockHash"
          + " WHERE sr.address = :stakeAddress"
          + " AND sr.txHash = :txHash"
          + " AND sr.type = :type")
  Optional<StakeHistoryProjection> findByAddressAndTx(
      @Param("stakeAddress") String stakeAddress,
      @Param("txHash") String txHash,
      @Param("type") CertificateType type);

  // TODO: findByTx query

  @Query(
      "SELECT CASE WHEN COUNT(*) > 0 THEN true ELSE false END FROM StakeRegistration sr WHERE sr.address = :stakeAddress AND sr.type = :type")
  Boolean existsByAddress(
      @Param("stakeAddress") String stakeAddress, @Param("type") CertificateType type);
}
