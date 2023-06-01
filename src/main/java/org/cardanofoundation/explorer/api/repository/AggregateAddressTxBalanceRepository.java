package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.common.AggregateAddressTxBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.Optional;

public interface AggregateAddressTxBalanceRepository extends JpaRepository<AggregateAddressTxBalance, Long> {

  @Query(value = "SELECT sum(aatb.balance) "
      + "FROM AggregateAddressTxBalance aatb "
      + "WHERE aatb.stakeAddressId = :stakeAddressId  "
      + "AND aatb.day <= :to")
  Optional<BigInteger> sumBalanceByStakeAddressId(@Param("stakeAddressId") Long stakeAddressId,
                                                  @Param("to") LocalDate to);

  @Transactional
  @Modifying
  @Query(
      value = "insert into agg_address_tx_balance (stake_address_id, address_id, balance, day) " +
          "SELECT addr.stake_address_id       as stake_address_id, " +
          "       addr.id                     as address_id, " +
          "       sum(addr.balance)           as sum_balance, " +
          "       date_trunc('day', atb.time) as time_agg " +
          "FROM address_tx_balance atb " +
          "inner join address addr on atb.address_id = addr.id " +
          "where atb.time >= :startOfDay " +
          "and atb.time <= :endOfDay " +
          "GROUP BY addr.id, time_agg " +
          "order by time_agg",
      nativeQuery = true)
  void analyzeDataByTime(@Param("startOfDay") Timestamp startOfDay,
                         @Param("endOfDay") Timestamp endOfDay);

  @Query(value = "SELECT sum(aatb.balance) "
      + "FROM AggregateAddressTxBalance aatb "
      + "WHERE aatb.addressId = :addressId  "
      + "AND aatb.day <= :to")
  Optional<BigInteger> sumBalanceByAddressId(@Param("addressId") Long addressId,
                                             @Param("to") LocalDate to);
}