package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface PoolDelegationSummaryProjection {

  String getPoolView();

  String getJson();

  BigDecimal getPledge();

  BigDecimal getFee();

  BigDecimal getPoolSize();

  Integer getOptimalPoolCount();

  BigDecimal getUtxo();

  Double getMargin();
}
