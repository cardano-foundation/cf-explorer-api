package org.cardanofoundation.explorer.api.repository;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.Optional;
import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressTxBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AggregateAddressTxBalanceRepository extends JpaRepository<AggregateAddressTxBalance, Long> {

  @Query(value = "SELECT sum(aatb.balance) "
      + "FROM AggregateAddressTxBalance aatb "
      + "WHERE aatb.stakeAddressId = :stakeAddressId  "
      + "AND aatb.day <= :to")
  Optional<BigInteger> sumBalanceByStakeAddressId(@Param("stakeAddressId") Long stakeAddressId,
                                                  @Param("to") LocalDate to);

  @Query(value = "SELECT sum(aatb.balance) "
      + "FROM AggregateAddressTxBalance aatb "
      + "WHERE aatb.addressId = :addressId  "
      + "AND aatb.day <= :to")
  Optional<BigInteger> sumBalanceByAddressId(@Param("addressId") Long addressId,
                                             @Param("to") LocalDate to);

  @Query("select max(a.day) from AggregateAddressTxBalance a")
  Optional<LocalDate> getMaxDay();
}