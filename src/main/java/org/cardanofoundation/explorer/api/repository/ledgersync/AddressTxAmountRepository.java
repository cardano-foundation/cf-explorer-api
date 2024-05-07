package org.cardanofoundation.explorer.api.repository.ledgersync;


import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressTxAmountId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTxAmount;

public interface AddressTxAmountRepository
    extends JpaRepository<AddressTxAmount, AddressTxAmountId> {

  @Query(
      value =
          """
          select :fromBalance + coalesce(min(calculated_balances.sum_balance), 0) as minVal,
                 :fromBalance + coalesce(max(calculated_balances.sum_balance), 0) as maxVal
          from (select sum(ata.quantity) over (order by ata.slot rows unbounded PRECEDING) as sum_balance
                from address_tx_amount ata
                where ata.address = :address
                  and ata.unit = 'lovelace'
                  and ata.block_time > :fromDate
                  and ata.block_time <= :toDate) as calculated_balances
          """, nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByAddress(
      @Param("address") String address,
      @Param("fromBalance") BigInteger fromBalance,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate);


  @Query(
      "SELECT sum(addressTxBalance.quantity) FROM AddressTxAmount addressTxBalance"
          + " WHERE addressTxBalance.address = :address"
          + " AND addressTxBalance.blockTime > :from and addressTxBalance.blockTime <= :to")
  Optional<BigInteger> getBalanceByAddressAndTime(@Param("address") String address,
                                                  @Param("from") Long from,
                                                  @Param("to") Long to);


  @Query(
      value =
      """
        SELECT sum(ata.quantity)
        FROM AddressTxAmount ata
        WHERE ata.address = :address
          AND ata.unit = 'lovelace'
          AND ata.blockTime <= :to
      """)
  Optional<BigInteger> sumBalanceByAddress(
      @Param("address") String address, @Param("to") Long to);
}
