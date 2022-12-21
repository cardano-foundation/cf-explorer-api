package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.AddressTxBalance;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  Integer countByAddress(String address);
  @Query("SELECT sum(addressTxBalance.balance) FROM AddressTxBalance addressTxBalance"
      + " WHERE addressTxBalance.address = :address"
      + " AND addressTxBalance.time < :time")
  BigDecimal getBalanceByAddressAndTime(String address, Timestamp time);

  @Query("SELECT addrTxBalance.balance FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address = :address"
      + " ORDER BY addrTxBalance.tx.id ASC")
  List<BigDecimal> findAllByAddress(String address);

}
