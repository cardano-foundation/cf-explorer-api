package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.stake.TrxBlockEpochStake;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardano.common.entity.StakeRegistration;
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
          + "JOIN Block bk ON bk.id = tx.block.id ",
      countQuery = "SELECT count(id) FROM StakeRegistration")
  Page<TrxBlockEpochStake> getDataForStakeRegistration(Pageable pageable);

  @Query("SELECT max(stakeRegis.tx.id) "
      + " FROM StakeRegistration stakeRegis"
      + " WHERE stakeRegis.addr = :stake")
  Optional<Long> findMaxTxIdByStake(StakeAddress stake);
}
