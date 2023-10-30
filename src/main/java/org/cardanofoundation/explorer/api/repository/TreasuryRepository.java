package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.InstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.TxInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Treasury;
import java.util.List;

import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface TreasuryRepository extends JpaRepository<Treasury, Long> {

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo, block.slotNo as slotNo,"
      + " block.blockNo as blockNo, tx.blockIndex as blockIndex, block.epochNo as epochNo,"
      + " treasury.amount as amount"
      + " FROM Treasury treasury"
      + " INNER JOIN Tx tx ON treasury.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON treasury.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  List<StakeInstantaneousRewardsProjection> getTreasuryByAddress(@Param("stakeKey") String stakeKey);

  @Query("SELECT stake.view as stakeAddress, treasury.amount as amount"
      + " FROM Treasury treasury"
      + " INNER JOIN StakeAddress stake ON treasury.addr = stake"
      + " WHERE treasury.tx = :tx"
      + " ORDER BY treasury.amount DESC")
  List<TxInstantaneousRewardsProjection> findByTx(@Param("tx") Tx tx);

  @Query("SELECT treasury.tx.id as txId, count(DISTINCT treasury.addr) as numberOfStakes, sum(treasury.amount) as rewards"
      + " FROM Treasury treasury"
      + " GROUP BY treasury.tx.id")
  List<InstantaneousRewardsProjection> findAllTx();
}
