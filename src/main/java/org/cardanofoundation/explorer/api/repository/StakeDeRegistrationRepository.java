package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.stake.TrxBlockEpochStake;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.StakeDeregistration;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface StakeDeRegistrationRepository extends JpaRepository<StakeDeregistration, Long> {

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.blockNo AS blockId, bk.epochNo AS epochNo, "
          + "bk.slotNo as slotNo, bk.epochSlotNo as epochSlotNo, sr.addr.view as stakeKey "
          + "FROM StakeDeregistration sr "
          + "JOIN Tx tx ON tx.id = sr.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "ORDER BY tx.blockId DESC, tx.blockIndex DESC",
      countQuery = "SELECT count(id) FROM StakeDeregistration")
  Page<TrxBlockEpochStake> getDataForStakeDeRegistration(Pageable pageable);

  @Query("SELECT max(stakeDeregis.tx.id) "
      + " FROM StakeDeregistration stakeDeregis"
      + " WHERE stakeDeregis.addr = :stake")
  Optional<Long> findMaxTxIdByStake(StakeAddress stake);


  @Query(value = "SELECT DISTINCT tx.hash as txHash, b.time as time,"
      + " b.epochSlotNo as epochSlotNo, b.blockNo as blockNo, b.epochNo as epochNo,"
      + " 'De Registered' AS action, tx.blockIndex as blockIndex"
      + " FROM StakeDeregistration sd"
      + " JOIN Tx tx ON tx.id = sd.tx.id"
      + " JOIN Block b ON b.id = tx.blockId"
      + " WHERE sd.addr.id = (SELECT sa.id FROM StakeAddress sa WHERE sa.view = :stakeKey)"
      + " ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<StakeHistoryProjection> getStakeDeRegistrationsByAddress(String stakeKey);
}
