package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface PoolDelegationSummaryProjection {
  Long getPoolId();
  String getJson();
  BigDecimal getPledge();
  BigDecimal getFee();
  BigDecimal getPoolSize();
}
