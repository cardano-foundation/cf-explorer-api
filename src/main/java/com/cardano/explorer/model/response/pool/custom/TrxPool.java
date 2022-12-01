package com.cardano.explorer.model.response.pool.custom;

import java.math.BigDecimal;

public interface TrxPool {

  BigDecimal getPledge();

  Double getMargin();

  BigDecimal getCost();

  Long getPoolId();
}
