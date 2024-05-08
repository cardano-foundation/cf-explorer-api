package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressTxAmountId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTxAmount;

public interface AddressTxAmountRepository extends JpaRepository<AddressTxAmount, AddressTxAmountId>{
  @Query(value = """
    SELECT tx.id AS txId, SUM(atm.quantity) AS amount, atm.blockTime AS time, atm.txHash as txHash, tx.validContract as validContract
    FROM AddressTxAmount atm
    INNER JOIN Tx tx on atm.txHash = tx.hash
    WHERE atm.unit = 'lovelace' AND atm.stakeAddress = :stakeAddressView
    GROUP BY tx.id, atm.blockTime, atm.txHash, tx.validContract
    """)
  Page<StakeTxProjection> findTxAndAmountByStake(
      @Param("stakeAddressView") String stakeAddressView, Pageable pageable);
}
