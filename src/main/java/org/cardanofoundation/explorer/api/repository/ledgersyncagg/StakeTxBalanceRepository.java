package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.math.BigInteger;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressTxBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeTxBalance;

public interface StakeTxBalanceRepository
    extends JpaRepository<StakeTxBalance, StakeAddressTxBalanceId> {

  @Query(
      value =
          """
        select :fromBalance + coalesce(min(calculated_balances.sum_balance), 0) as minVal,
               :fromBalance + coalesce(max(calculated_balances.sum_balance), 0) as maxVal
        from (select sum(stb.balance_change) over (order by stb.slot rows unbounded PRECEDING) as sum_balance
              from stake_tx_balance stb
              where stb.stake_address = :stakeAddress
                and stb.slot > :fromSlot
                and stb.slot <= :toSlot) as calculated_balances
        """,
      nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByStakeAddress(
      @Param("stakeAddress") String stakeAddress,
      @Param("fromBalance") BigInteger fromBalance,
      @Param("fromSlot") Long fromSlot,
      @Param("toSlot") Long toSlot);

  @Query(
      value =
          "select MAX(stb.slot)"
              + " from StakeTxBalance stb "
              + " where stb.stakeAddress = :stakeAddress ")
  Optional<Long> findMaxSlotByStakeAddress(@Param("stakeAddress") String stakeAddress);
}
