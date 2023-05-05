package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Treasury;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface TreasuryRepository extends JpaRepository<Treasury, Long> {

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, tx.blockIndex as blockIndex, block.epochNo as epochNo,"
      + " treasury.amount as amount"
      + " FROM Treasury treasury"
      + " INNER JOIN Tx tx ON treasury.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON treasury.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  List<StakeInstantaneousRewardsProjection> getTreasuryByAddress(String stakeKey);
}
