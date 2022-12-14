package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface TxPoolProjection {

  Long getBlockId();

  BigDecimal getPledge();

  Double getMargin();

  BigDecimal getCost();

  Long getPoolId();

  String getPoolName();
}
