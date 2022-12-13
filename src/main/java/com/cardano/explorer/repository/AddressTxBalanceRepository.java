package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.AddressTxBalance;
import java.math.BigDecimal;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTxBalanceRepository extends JpaRepository<AddressTxBalance, Long> {

  Integer countByAddress(String address);

  @Query("SELECT COALESCE(sum(balance), 0) FROM AddressTxBalance WHERE address = :address")
  BigDecimal getBalanceByAddress(String address);

}
