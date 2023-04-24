package com.cardano.explorer.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface PoolDelegationSummaryProjection {

  Long getPoolId();

  String getPoolView();

  String getPoolName();

  BigInteger getPledge();

  BigInteger getFee();

  Integer getOptimalPoolCount();

  Double getMargin();

  BigInteger getReserves();
}
