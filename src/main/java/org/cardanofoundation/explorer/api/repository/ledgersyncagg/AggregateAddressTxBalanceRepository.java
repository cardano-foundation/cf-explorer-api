package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AggregateAddressBalanceProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AggAddressTxBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AggregateAddressTxBalance;

public interface AggregateAddressTxBalanceRepository
    extends JpaRepository<AggregateAddressTxBalance, AggAddressTxBalanceId> {

  @Query(
      value =
          "SELECT aatb "
              + "FROM AggregateAddressTxBalance aatb "
              + "WHERE aatb.address = :address "
              + "AND aatb.day >= :from "
              + "AND aatb.day < :to "
              + "ORDER BY aatb.day ASC")
  List<AggregateAddressTxBalance> findAllByAddressIdAndDayBetween(
      @Param("address") String address, @Param("from") LocalDate from, @Param("to") LocalDate to);

  @Query(
      value =
          "SELECT sum(aatb.balance) as balance, aatb.day as day "
              + "FROM AggregateAddressTxBalance aatb "
              + "WHERE aatb.stakeAddress = :stakeAddressId "
              + "AND aatb.day >= :from "
              + "AND aatb.day < :to "
              + "GROUP BY aatb.day "
              + "ORDER BY aatb.day ASC")
  List<AggregateAddressBalanceProjection> findAllByStakeAddressIdAndDayBetween(
      @Param("stakeAddress") String stakeAddress,
      @Param("from") LocalDate from,
      @Param("to") LocalDate to);
}
