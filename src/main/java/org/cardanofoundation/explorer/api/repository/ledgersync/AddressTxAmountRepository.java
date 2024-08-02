package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AddressQuantityDayProjection;
import org.cardanofoundation.explorer.api.projection.AddressTxCountProjection;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.api.projection.TxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressTxAmountId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTxAmount;

public interface AddressTxAmountRepository
    extends JpaRepository<AddressTxAmount, AddressTxAmountId> {

  @Query(
      value =
          """
                  SELECT ab.address as address, COUNT(ab.address) as txCount FROM AddressBalance ab
                  WHERE ab.address IN :addresses GROUP BY ab.address
                  """)
  List<AddressTxCountProjection> getTxCountListForAddresses(
      @Param("addresses") Collection<String> addressList);

  @Query(
      value =
          """
                SELECT COUNT(ab.address) FROM AddressTxAmount ab
                  WHERE ab.address = :address GROUP BY ab.address
                """)
  Optional<Long> getTxCountForAddress(@Param("address") String address);

  @Query(
      value =
          """
                SELECT COUNT(ab.stakeAddress) FROM AddressTxAmount ab
                  WHERE ab.stakeAddress = :stakeAddress
                """)
  Optional<Long> getTxCountForStakeAddress(@Param("stakeAddress") String stakeAddress);

  @Query(
      value =
          """
          select :fromBalance + coalesce(min(calculated_balances.sum_balance), 0) as minVal,
                 :fromBalance + coalesce(max(calculated_balances.sum_balance), 0) as maxVal
          from (select sum(ata.quantity) over (order by ata.slot rows unbounded PRECEDING) as sum_balance
                from address_tx_amount ata
                where ata.address = :address
                  and ata.unit = 'lovelace'
                  and ata.slot > :fromDate
                  and ata.slot <= :toDate) as calculated_balances
          """,
      nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByAddressAndSlotRange(
      @Param("address") String address,
      @Param("fromBalance") BigInteger fromBalance,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate);

  @Query(
      "SELECT sum(addressTxBalance.quantity) FROM AddressTxAmount addressTxBalance"
          + " WHERE addressTxBalance.address = :address"
          + " AND addressTxBalance.slot > :from and addressTxBalance.slot <= :to")
  Optional<BigInteger> getBalanceByAddressAndSlotRange(
      @Param("address") String address, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
        SELECT sum(ata.quantity)
        FROM AddressTxAmount ata
        WHERE ata.address = :address
          AND ata.unit = 'lovelace'
          AND ata.slot <= :to
      """)
  Optional<BigInteger> sumBalanceByAddressToSlot(
      @Param("address") String address, @Param("to") Long to);

  @Query(
      value =
          """
    SELECT sum(ata.quantity) FROM AddressTxAmount ata
    WHERE ata.unit = :unit
    AND ata.quantity > 0
    AND ata.slot > :from
    AND ata.slot < :to
    """)
  Optional<BigInteger> sumBalanceBetweenSlotRange(
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
              AND ata.slot <= :to
          """)
  Optional<BigInteger> sumBalanceByStakeAddressToSlot(
      @Param("stakeAddress") String stakeAddress, @Param("to") Long to);

  @Query(
      "SELECT sum(addressTxAmount.quantity) FROM AddressTxAmount addressTxAmount"
          + " WHERE addressTxAmount.stakeAddress = :stakeAddress"
          + " AND addressTxAmount.unit = 'lovelace'"
          + " AND addressTxAmount.slot > :from and addressTxAmount.slot <= :to")
  Optional<BigInteger> getBalanceByStakeAddressAndSlotRange(
      @Param("stakeAddress") String stakeAddress, @Param("from") Long from, @Param("to") Long to);

  @Query(
      "SELECT sum(addressTxAmount.quantity) FROM AddressTxAmount addressTxAmount"
          + " WHERE addressTxAmount.stakeAddress = :stakeAddress"
          + " AND addressTxAmount.unit = 'lovelace'"
          + " AND addressTxAmount.blockTime <= :time")
  Optional<BigInteger> getBalanceByStakeAddressAndSlotRange(
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
    AND atm.slot > :fromDate AND atm.slot <= :toDate
    GROUP BY tx.id, atm.blockTime, atm.txHash, tx.validContract
    """)
  Page<StakeTxProjection> findTxAndAmountByStakeAndSlotRange(
      @Param("stakeAddressView") String stakeAddressView,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate,
      Pageable pageable);

  @Query(
      value =
          """
              SELECT tmp.txHash as txHash, tmp.blockTime as blockTime
              FROM
              (SELECT DISTINCT(atm.txHash) as txHash, atm.blockTime as blockTime
              FROM AddressTxAmount atm WHERE atm.address = :address) as tmp
              """)
  List<TxProjection> findAllTxByAddress(@Param("address") String address, Pageable pageable);

  @Query(
      value =
          """
              SELECT tmp.txHash as txHash, tmp.blockTime as blockTime
              FROM
              (SELECT DISTINCT(atm.txHash) as txHash, atm.blockTime as blockTime
                            FROM AddressTxAmount atm WHERE atm.stakeAddress = :stakeAddress) as tmp
              """)
  List<TxProjection> findAllTxByStakeAddress(
      @Param("stakeAddress") String stakeAddress, Pageable pageable);

  @Query(
      value =
          """
              SELECT DISTINCT(atm.txHash) as txHash, atm.slot as blockTime
              FROM AddressTxAmount atm WHERE atm.unit = :unit
              """)
  List<TxProjection> findAllTxByUnit(@Param("unit") String unit, Pageable pageable);

  @Query(
      value =
          """
              SELECT atm FROM AddressTxAmount atm
              WHERE atm.address = :address
              AND atm.txHash IN :txHashes
          """)
  List<AddressTxAmount> findAllByAddressAndTxHashIn(
      @Param("address") String address, @Param("txHashes") List<String> txHashes);

  @Query(
      value =
          """
              SELECT atm FROM AddressTxAmount atm
              WHERE atm.stakeAddress = :stakeAddress
              AND atm.txHash IN :txHashes
          """)
  List<AddressTxAmount> findAllByStakeAddressAndTxHashIn(
      @Param("stakeAddress") String stakeAddress, @Param("txHashes") List<String> txHashes);

  @Query(
      value =
          """
              SELECT max(atm.slot) FROM AddressTxAmount atm
              WHERE atm.unit = :unit
          """)
  Long getLastSlotOfToken(@Param("unit") String unit);

  @Query(
      value =
          """
              SELECT max(atm.blockTime) FROM AddressTxAmount atm
              WHERE atm.slot = :slot
          """)
  Long getBlockTime(@Param("slot") Long slot);

  @Query(
          value =
                  """
                      SELECT max(atm.blockTime) FROM AddressTxAmount atm
                      WHERE atm.unit = :unit
                  """)
  Long getLastActivityTimeOfToken(@Param("unit") String unit);

  @Query(
      value =
          """
              SELECT SUM(atm.quantity) as quantity, date_trunc('day', to_timestamp(atm.block_time) ) as day
              FROM address_tx_amount atm
              WHERE atm.address = :address
              AND atm.slot >= :from
              AND atm.slot <= :to
              GROUP BY day
          """,
      nativeQuery = true)
  List<AddressQuantityDayProjection> findAllByAddressAndSlotBetween(
      @Param("address") String address, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
                  SELECT SUM(atm.quantity) as quantity, date_trunc('day', to_timestamp(atm.block_time)) as day
                  FROM address_tx_amount atm
                  WHERE atm.stake_address = :stakeAddress
                  AND atm.slot >= :from
                  AND atm.slot <= :to
                  GROUP BY day
                  """,
      nativeQuery = true)
  List<AddressQuantityDayProjection> findAllByStakeAddressAndSlotBetween(
      @Param("stakeAddress") String stakeAddress, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
                      SELECT SUM(atm.quantity) as quantity, date_trunc('day', to_timestamp(atm.block_time )) as day
                      FROM address_tx_amount atm
                      INNER JOIN multi_asset ma ON atm.unit = ma.unit
                      WHERE ma.id = :tokenId
                      AND atm.slot >= :from
                      AND atm.slot <= :to
                      GROUP BY day
                  """,
      nativeQuery = true)
  List<AddressQuantityDayProjection> findAllByTokenIdAndSlotBetween(
      @Param("tokenId") Long tokenId, @Param("from") Long from, @Param("to") Long to);

  @Query(
      value =
          """
                  SELECT MAX(tx.id) FROM AddressTxAmount atm INNER JOIN Tx tx ON atm.txHash = tx.hash
                  WHERE atm.stakeAddress = :stakeAddress
                  """)
  Optional<Long> findMaxTxIdByStakeAddress(@Param("stakeAddress") String stakeAddress);
}
