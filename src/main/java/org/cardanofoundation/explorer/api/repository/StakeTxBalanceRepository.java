package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeTxBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Optional;

public interface StakeTxBalanceRepository extends JpaRepository<StakeTxBalance, Long> {

  @Query(value = "select :fromBalance + coalesce(min(calculated_balances.sum_balance), 0) as minVal, "
      + "               :fromBalance + coalesce(max(calculated_balances.sum_balance), 0) as maxVal "
      + " from (select sum(stb.balance_change) over (order by stb.tx_id rows unbounded PRECEDING) as sum_balance "
      + "       from stake_tx_balance stb "
      + "       where stb.stake_address_id = :addressId "
      + "       and stb.time > :fromDate and stb.time <= :toDate ) as calculated_balances", nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByStakeAddress(@Param("addressId") Long addressId,
                                              @Param("fromBalance") BigInteger fromBalance,
                                              @Param("fromDate") Timestamp fromDate,
                                              @Param("toDate") Timestamp toDate);

  @Query(value = "select MAX(stb.txId)"
      + " from StakeTxBalance stb "
      + " where stb.stakeAddressId = :stakeAddressId ")
  Optional<Long> findMaxTxIdByStakeAddressId(@Param("stakeAddressId") Long stakeAddressId);
}
