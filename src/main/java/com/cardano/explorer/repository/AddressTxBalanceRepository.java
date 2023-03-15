package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.AddressTxBalance;
import com.sotatek.cardano.common.entity.Tx;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  @Query("SELECT count(addressTxBalance) FROM AddressTxBalance addressTxBalance"
      + " INNER JOIN Address addr ON addressTxBalance.address = addr"
      + " WHERE addr.address = :address")
  Long countByAddress(String address);
  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " INNER JOIN Address addr ON addressTxBalance.address = addr"
      + " WHERE addr.address = :address"
      + " AND addressTxBalance.time < :time")
  BigDecimal getBalanceByAddressAndTime(String address, Timestamp time);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Address addr ON addrTxBalance.address = addr"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " WHERE addr.address = :address"
      + " ORDER BY addrTxBalance.tx.blockId ASC, addrTxBalance.tx.blockIndex ASC")
  List<BigDecimal> findAllByAddress(String address);

  @Query(value = "SELECT tx FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Tx tx ON addrTxBalance.tx = tx"
      + " INNER JOIN Address addr ON addrTxBalance.address = addr"
      + " WHERE addr.address = :address"
      + " ORDER BY tx.blockId DESC, tx.blockIndex DESC",
      countQuery = "SELECT count(addrTxBalance) "
      + " FROM AddressTxBalance addrTxBalance"
      + " INNER JOIN Address addr ON addrTxBalance.address = addr"
      + " WHERE addr.address = :address")
  Page<Tx> findAllByAddress(String address, Pageable pageable);

}
