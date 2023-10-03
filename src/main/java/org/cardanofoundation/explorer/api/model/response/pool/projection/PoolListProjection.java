package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;


public interface PoolListProjection {

  Long getPoolId();

  String getPoolView();

  String getPoolName();

  BigInteger getPledge();

  BigInteger getFee();

  Double getMargin();
  Integer getPoolNameLength();

  Integer getNumberDelegators();

  Integer getEpochBlock();

  Integer getLifetimeBlock();
}
