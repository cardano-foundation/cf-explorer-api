package com.cardano.explorer.repository;

import com.cardano.explorer.projection.StakeTreasuryProjection;
import com.sotatek.cardano.common.entity.Treasury;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface TreasuryRepository extends JpaRepository<Treasury, Long> {

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, treasury.amount as amount"
      + " FROM Treasury treasury"
      + " INNER JOIN Tx tx ON treasury.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON treasury.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeTreasuryProjection> getTreasuryByAddress(String stakeKey, Pageable pageable);
}
