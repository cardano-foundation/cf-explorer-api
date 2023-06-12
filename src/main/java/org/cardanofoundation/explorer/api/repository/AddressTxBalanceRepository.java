package org.cardanofoundation.explorer.api.repository;

import java.util.Collection;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  @Query("SELECT count(addressTxBalance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address")
  Long countByAddress(@Param("address") Address address);
  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address"
      + " AND addressTxBalance.time <= :time")
  BigInteger getBalanceByAddressAndTime(@Param("address") Address address, @Param("time") Timestamp time);

  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address"
      + " AND addressTxBalance.time > :from and addressTxBalance.time <= :to")
  Optional<BigInteger> getBalanceByAddressAndTime(@Param("address") Address address,
                                                  @Param("from") Timestamp from,
                                                  @Param("to") Timestamp to);

  @Query(value = "select min(calculated_balances.sum_of_no) as minVal, max(calculated_balances.sum_of_no) as maxVal " +
      "from (select list_balance.id, sum(list_balance.balance) OVER (order by list_balance.id) as sum_of_no " +
      "      from (select ROW_NUMBER() over (order by tx.block_id asc, tx.block_index asc) AS id, atb.balance " +
      "            from address_tx_balance atb " +
      "            inner join tx tx on tx.id = atb.tx_id " +
      "            where atb.address_id = :addressId " +
      "            order by tx.block_id asc, tx.block_index asc) as list_balance" +
      "    ) as calculated_balances", nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByAddress(@Param("addressId") Long addressId);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigInteger> findAllByAddress(@Param("address") Address address);

  @Query(value = "SELECT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address")
  List<Tx> findAllByAddress(@Param("address") Address address, Pageable pageable);

  @Query(value = "select min(calculated_balances.sum_of_no) as minVal, max(calculated_balances.sum_of_no) as maxVal " +
      "from (select list_balance.id, sum(list_balance.balance) OVER (order by list_balance.id) as sum_of_no " +
      "      from (select ROW_NUMBER() over (order by tx.block_id asc, tx.block_index asc) AS id, atb.balance " +
      "            from address_tx_balance atb " +
      "            inner join tx tx on tx.id = atb.tx_id " +
      "            inner join address addr on atb.address_id = addr.id and addr.is_deleted = false " +
      "                                              and addr.stake_address_id = :stakeAddressId " +
      "            order by tx.block_id asc, tx.block_index asc) as list_balance" +
      "    ) as calculated_balances", nativeQuery = true)
  MinMaxProjection findMinMaxBalanceByStakeAddress(@Param("stakeAddressId") Long stakeAddressId);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress = :stakeAddress)"
      + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigInteger> findAllByStakeAddress(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(value = "SELECT DISTINCT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.stakeAddress.id = :stakeAddressId "
      + " ORDER BY tx.id DESC")
  Page<Tx> findAllByStake(@Param("stakeAddressId") Long stakeAddressId, Pageable pageable);

  @Query(value = "SELECT addrTxBalance.tx.id as txId, sum(addrTxBalance.balance) as amount,"
      + " addrTxBalance.time as time"
      + " FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress)"
      + " GROUP BY addrTxBalance.tx.id, addrTxBalance.time")
  Page<StakeTxProjection> findTxAndAmountByStake(@Param("stakeAddress") String stakeAddress, Pageable pageable);

  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress = :stakeAddress)"
      + " AND addressTxBalance.time <= :time")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(@Param("stakeAddress") StakeAddress stakeAddress,
                                                       @Param("time") Timestamp time);

  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress = :stakeAddress)"
      + " AND addressTxBalance.time > :from and addressTxBalance.time <= :to")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(@Param("stakeAddress") StakeAddress stakeAddress,
                                                       @Param("from") Timestamp from,
                                                       @Param("to") Timestamp to);

  @Query("SELECT addressTxBalance FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.tx.id in :ids and addressTxBalance.address.address = :address")
  List<AddressTxBalance> findByTxIdInAndByAddress(@Param("ids") Collection<Long> ids,
                                                  @Param("address") String address);

  @Query("SELECT addressTxBalance FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.tx.id in :ids and addressTxBalance.stakeAddress.id = :stakeId")
  List<AddressTxBalance> findByTxIdInAndStakeId(@Param("ids") Collection<Long> ids,
                                                @Param("stakeId") Long stakeId);

  @Query(value = "SELECT addrTxBalance.tx.id as txId, sum(addrTxBalance.balance) as amount,"
      + " addrTxBalance.time as time"
      + " FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress)"
      + " AND addrTxBalance.time >= :fromDate AND addrTxBalance.time <= :toDate"
      + " GROUP BY addrTxBalance.tx.id, addrTxBalance.time")
  Page<StakeTxProjection> findTxAndAmountByStakeAndDateRange(@Param("stakeAddress") String stakeAddress,
                                                 @Param("fromDate") Timestamp fromDate,
                                                 @Param("toDate") Timestamp toDate,
                                                 Pageable pageable);
}
