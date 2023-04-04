package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.AddressTxBalance;
import com.sotatek.cardano.common.entity.Tx;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  @Query("SELECT count(addressTxBalance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address")
  Long countByAddress(Address address);
  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address"
      + " AND addressTxBalance.time < :time")
  BigInteger getBalanceByAddressAndTime(Address address, Timestamp time);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigInteger> findAllByAddress(Address address);

  @Query(value = "SELECT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY tx.blockId DESC, tx.blockIndex DESC")
  List<Tx> findAllByAddress(Address address, Pageable pageable);

}
