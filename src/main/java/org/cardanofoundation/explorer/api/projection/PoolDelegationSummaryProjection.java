package org.cardanofoundation.explorer.api.projection;

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
