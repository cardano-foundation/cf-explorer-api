package com.cardano.explorer.model.response.pool.custom;

import java.math.BigDecimal;
import java.sql.Timestamp;

public interface PoolDetailDelegator {

  Long getId();

  String getAddress();

  Timestamp getTime();

  BigDecimal getFee();
}
