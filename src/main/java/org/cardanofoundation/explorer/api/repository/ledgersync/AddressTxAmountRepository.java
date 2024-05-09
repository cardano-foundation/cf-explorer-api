package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
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
          """,
      nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByAddress(
      @Param("address") String address,
      @Param("fromBalance") BigInteger fromBalance,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate);

  @Query(
      "SELECT sum(addressTxBalance.quantity) FROM AddressTxAmount addressTxBalance"
          + " WHERE addressTxBalance.address = :address"
          + " AND addressTxBalance.blockTime > :from and addressTxBalance.blockTime <= :to")
  Optional<BigInteger> getBalanceByAddressAndTime(
      @Param("address") String address, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
        SELECT sum(ata.quantity)
        FROM AddressTxAmount ata
        WHERE ata.address = :address
          AND ata.unit = 'lovelace'
          AND ata.blockTime <= :to
      """)
  Optional<BigInteger> sumBalanceByAddress(@Param("address") String address, @Param("to") Long to);

  @Query(
      value =
          """
    SELECT sum(ata.quantity) FROM AddressTxAmount ata
    WHERE ata.unit = :unit
    AND ata.quantity > 0
    AND ata.blockTime > :from
    AND ata.blockTime < :to
    """)
  Optional<BigInteger> sumBalanceBetweenTime(
      @Param("unit") String unit, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
      SELECT tx.id                     as txId,
             sum(addTxAmount.quantity) as amount,
             addTxAmount.blockTime     as time
      FROM AddressTxAmount addTxAmount
               JOIN Tx tx ON addTxAmount.txHash = tx.hash
      WHERE tx.id > :fromTxId
        AND addTxAmount.stakeAddress = :stakeAddress
      GROUP BY tx.id, addTxAmount.blockTime
      """)
  List<StakeTxProjection> findTxAndAmountByStake(
      @Param("stakeAddress") String stakeAddress, @Param(("fromTxId")) Long fromTxId);

  @Query(
      value =
          """
            SELECT sum(ata.quantity)
            FROM AddressTxAmount ata
            WHERE ata.stakeAddress = :stakeAddress
              AND ata.unit = 'lovelace'
              AND ata.blockTime <= :to
          """)
  Optional<BigInteger> sumBalanceByStakeAddress(
      @Param("stakeAddress") String stakeAddress, @Param("to") Long to);

  @Query(
      "SELECT sum(addressTxAmount.quantity) FROM AddressTxAmount addressTxAmount"
          + " WHERE addressTxAmount.stakeAddress = :stakeAddress"
          + " AND addressTxAmount.unit = 'lovelace'"
          + " AND addressTxAmount.blockTime > :from and addressTxAmount.blockTime <= :to")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(
      @Param("stakeAddress") String stakeAddress, @Param("from") Long from, @Param("to") Long to);

  @Query(
      "SELECT sum(addressTxAmount.quantity) FROM AddressTxAmount addressTxAmount"
          + " WHERE addressTxAmount.stakeAddress = :stakeAddress"
          + " AND addressTxAmount.unit = 'lovelace'"
          + " AND addressTxAmount.blockTime <= :time")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(
      @Param("stakeAddress") String stakeAddress, @Param("time") Long time);

  @Query(
      value =
          """
    SELECT tx.id AS txId, SUM(atm.quantity) AS amount, atm.blockTime AS time, atm.txHash as txHash, tx.validContract as validContract
    FROM AddressTxAmount atm
    INNER JOIN Tx tx on atm.txHash = tx.hash
    WHERE atm.unit = 'lovelace' AND atm.stakeAddress = :stakeAddressView
    GROUP BY tx.id, atm.blockTime, atm.txHash, tx.validContract
    """)
  Page<StakeTxProjection> findTxAndAmountByStake(
      @Param("stakeAddressView") String stakeAddressView, Pageable pageable);

  @Query(
      value =
          """
    SELECT tx.id AS txId, SUM(atm.quantity) AS amount, atm.blockTime AS time, atm.txHash as txHash, tx.validContract as validContract
    FROM AddressTxAmount atm
    INNER JOIN Tx tx on atm.txHash = tx.hash
    WHERE atm.unit = 'lovelace' AND atm.stakeAddress = :stakeAddressView
    AND atm.blockTime > :fromDate AND atm.blockTime <= :toDate
    GROUP BY tx.id, atm.blockTime, atm.txHash, tx.validContract
    """)
  Page<StakeTxProjection> findTxAndAmountByStakeAndDateRange(
      @Param("stakeAddressView") String stakeAddressView,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate,
      Pageable pageable);
}
