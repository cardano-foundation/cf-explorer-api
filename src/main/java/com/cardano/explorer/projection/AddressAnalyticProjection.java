package com.cardano.explorer.projection;

import java.math.BigDecimal;
import java.sql.Timestamp;

public interface AddressAnalyticProjection {
  Timestamp getTime();
  BigDecimal getBalance();
}
