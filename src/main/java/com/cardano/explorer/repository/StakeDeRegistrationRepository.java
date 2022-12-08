package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.custom.TrxBlockEpoch;
import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.sotatek.cardano.common.entity.StakeDeregistration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface StakeDeRegistrationRepository extends JpaRepository<StakeDeregistration, Long> {

  @Query(value =
      "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.id AS blockId, bk.epochNo AS epochNo, bk.slotNo as slotNo "
          + "FROM StakeDeregistration sr "
          + "JOIN Tx tx ON tx.id = sr.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "GROUP BY tx.id, tx.hash, bk.time, bk.id ")
  Page<TrxBlockEpoch> getDataForPoolDeRegistration(Pageable pageable);

  @Query(value = "SELECT tx.id AS txId, tx.hash AS txHash, bk.time AS txTime, bk.id AS blockId, bk.epochNo AS epochNo, " +
          "bk.slotNo as slotNo, sr.addr.view as stakeKey "
          + "FROM StakeDeregistration sr "
          + "JOIN Tx tx ON tx.id = sr.tx.id "
          + "JOIN Block bk ON bk.id = tx.block.id "
          + "GROUP BY tx.id, tx.hash, bk.time, bk.id, sr.addr.view ")
  Page<TrxBlockEpochStake> getDataForStakeDeRegistration(Pageable pageable);
}
