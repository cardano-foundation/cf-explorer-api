package org.cardanofoundation.explorer.api.repository;

import java.util.Collection;
import java.util.Set;
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
  BigInteger getBalanceByAddressAndTime(
      @Param("address") Address address, @Param("time") Timestamp time);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigInteger> findAllByAddress(@Param("address") Address address);

  @Query(value = "SELECT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY tx.blockId DESC, tx.blockIndex DESC")
  List<Tx> findAllByAddress(@Param("address") Address address, Pageable pageable);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
          + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
          + " WHERE addrTxBalance.address IN "
          + " (SELECT addr FROM Address addr WHERE addr.stakeAddress = :stakeAddress)"
          + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigInteger> findAllByStakeAddress(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(value = "SELECT DISTINCT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress)"
      + " ORDER BY tx.blockId DESC, tx.blockIndex DESC")
  Page<Tx> findAllByStake(@Param("stakeAddress") String stakeAddress, Pageable pageable);

  @Query(value = "SELECT addrTxBalance.tx.id as txId, sum(addrTxBalance.balance) as amount,"
      + " addrTxBalance.time as time"
      + " FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress.view = :stakeAddress)"
      + " GROUP BY addrTxBalance.tx.id, addrTxBalance.time")
  Page<StakeTxProjection> findTxAndAmountByStake(
      @Param("stakeAddress") String stakeAddress, Pageable pageable);

  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address IN "
      + " (SELECT addr FROM Address addr WHERE addr.stakeAddress = :stakeAddress)"
      + " AND addressTxBalance.time <= :time")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(
      @Param("stakeAddress") StakeAddress stakeAddress, @Param("time") Timestamp time);

  @Query("SELECT addressTxBalance FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.tx.id in :ids and addressTxBalance.address.address = :address")
  List<AddressTxBalance> findByTxIdInAndByAddress(@Param("ids") Collection<Long> ids,
                                                  @Param("address") String address);
  @Query("SELECT addressTxBalance FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.tx.id in :ids and addressTxBalance.addressId in :addressIds")
  List<AddressTxBalance> findByTxIdInAndByAddressIn(@Param("ids") Collection<Long> ids,
                                                    @Param("addressIds") Set<Long> addressIds);
}
