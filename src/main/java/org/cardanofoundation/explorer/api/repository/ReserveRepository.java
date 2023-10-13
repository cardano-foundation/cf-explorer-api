package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.InstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.StakeInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.api.projection.TxInstantaneousRewardsProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Reserve;
import java.util.List;

import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ReserveRepository extends JpaRepository<Reserve, Long> {

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo, block.slotNo as slotNo,"
      + " block.blockNo as blockNo, tx.blockIndex as blockIndex, block.epochNo as epochNo, "
      + " reserve.amount as amount"
      + " FROM Reserve reserve"
      + " INNER JOIN Tx tx ON reserve.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON reserve.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  List<StakeInstantaneousRewardsProjection> getReserveByAddress(@Param("stakeKey") String stakeKey);


  @Query("SELECT stake.view as stakeAddress, reserve.amount as amount"
          + " FROM Reserve reserve"
          + " INNER JOIN StakeAddress stake ON reserve.addr = stake"
          + " WHERE reserve.tx = :tx"
          + " ORDER BY reserve.amount DESC")
  List<TxInstantaneousRewardsProjection> findByTx(@Param("tx") Tx tx);

  @Query("SELECT reserve.tx.id as txId, count(DISTINCT reserve.addr) as numberOfStakes, sum(reserve.amount) as rewards"
      + " FROM Reserve reserve"
      + " GROUP BY reserve.tx.id")
  List<InstantaneousRewardsProjection> findAllTx();
}
