package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.AddressTxBalance;
import java.math.BigDecimal;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  Integer countByAddress(String address);

  @Query("SELECT MAX(addrTxBalance.balance) "
      + " FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address = :address")
  BigDecimal getMaxBalanceByAddress(String address);

  @Query("SELECT MIN(addrTxBalance.balance) "
      + " FROM AddressTxBalance addrTxBalance"
      + " WHERE addrTxBalance.address = :address")
  BigDecimal getMinBalanceByAddress(String address);

}
