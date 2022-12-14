package com.cardano.explorer.repository.custom;

import java.math.BigDecimal;
import java.sql.Timestamp;

public interface CustomAddressTxBalanceRepository {

  public BigDecimal getBalanceByAddressAndTime(String address, Timestamp time);
}
