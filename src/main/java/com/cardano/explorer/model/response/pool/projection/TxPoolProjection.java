package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface TxPoolProjection {

  BigDecimal getPledge();

  Double getMargin();

  BigDecimal getCost();

  Long getPoolId();
}
