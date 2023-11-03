package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.cardanofoundation.explorer.consumercommon.entity.StakeRegistration_;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface StakeRegistrationRepository extends JpaRepository<StakeRegistration, Long> {

  @Query("SELECT max(stakeRegis.tx.id) "
      + " FROM StakeRegistration stakeRegis"
      + " WHERE stakeRegis.addr = :stake")
  Optional<Long> findMaxTxIdByStake(@Param("stake") StakeAddress stake);

  @Query(value = "SELECT tx.hash as txHash, b.time as time, b.slotNo as slotNo,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " 'Registered' AS action, tx.blockIndex as blockIndex, tx.fee as fee, tx.deposit as deposit"
      + " FROM StakeRegistration sr"
      + " JOIN Tx tx ON tx.id = sr.tx.id"
      + " JOIN Block b ON b.id = tx.blockId"
      + " WHERE sr.addr = :stakeKey"
      + " ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<StakeHistoryProjection> getStakeRegistrationsByAddress(@Param("stakeKey") StakeAddress stakeKey);

  @Query(value = "SELECT sr.tx.id"
      + " FROM StakeRegistration sr"
      + " WHERE sr.addr = :stakeKey AND sr.tx.id IN :txIds")
  List<Long> getStakeRegistrationsByAddressAndTxIn(@Param("stakeKey") StakeAddress stakeKey,
                                                   @Param("txIds") Collection<Long> txIds);

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
  Page<StakeHistoryProjection> getStakeRegistrationsByAddress(
      @Param("stakeKey") StakeAddress stakeKey, @Param("txHash") String txHash,
      @Param("fromTime") Timestamp fromTime, @Param("toTime") Timestamp toTime, Pageable pageable);

  @Query(value = "SELECT tx.hash as txHash, b.time as time, b.epochNo as epochNo,"
      + " tx.fee as fee, tx.deposit as deposit"
      + " FROM StakeRegistration sr"
      + " JOIN Tx tx ON tx.id = sr.tx.id"
      + " JOIN Block b ON b.id = tx.blockId"
      + " JOIN StakeAddress sa ON sa.id = sr.addr.id"
      + " WHERE sa.view = :stakeKey"
      + " AND tx.hash = :txHash")
  Optional<StakeHistoryProjection> findByAddressAndTx(
      @Param("stakeKey") String stakeKey, @Param("txHash") String txHash);

  @EntityGraph(attributePaths = {StakeRegistration_.ADDR})
  List<StakeRegistration> findByTx(@Param("tx") Tx tx);

  Boolean existsByAddr(@Param("stakeAddress") StakeAddress stakeAddress);
}
