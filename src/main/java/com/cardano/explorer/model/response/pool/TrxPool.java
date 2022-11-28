package com.cardano.explorer.model.response.pool;

import java.math.BigDecimal;

public interface TrxPool {

  BigDecimal getPledge();

  Double getMargin();

  BigDecimal getCost();

  Long getPoolId();
}
