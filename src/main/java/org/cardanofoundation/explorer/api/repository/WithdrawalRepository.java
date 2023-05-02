package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal_;
import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface WithdrawalRepository extends JpaRepository<Withdrawal, Long> {
  @EntityGraph(attributePaths = {Withdrawal_.ADDR})
  List<Withdrawal> findByTx(Tx tx);

  @Query("SELECT SUM(w.amount) FROM Withdrawal w "
      + " INNER JOIN StakeAddress stakeAddress ON w.addr.id = stakeAddress.id"
      + " WHERE stakeAddress.view = :stakeAddress")
  Optional<BigInteger> getRewardWithdrawnByStakeAddress(String stakeAddress);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, withdrawal.amount as amount"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON withdrawal.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeWithdrawalProjection> getWithdrawalByAddress(String stakeKey, Pageable pageable);
}
