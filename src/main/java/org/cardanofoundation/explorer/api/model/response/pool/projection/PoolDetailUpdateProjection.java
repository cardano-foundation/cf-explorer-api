package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface PoolDetailUpdateProjection {

  Long getPoolId();

  String getHashRaw();

  String getPoolName();

  String getTickerName();

  BigInteger getCost();

  Double getMargin();

  BigInteger getPledge();

  Integer getParamK();

  BigInteger getReserves();

  String getRewardAddress();
  Integer getEpochBlock();
  Integer getLifetimeBlock();
  Integer getDelegators();
}
