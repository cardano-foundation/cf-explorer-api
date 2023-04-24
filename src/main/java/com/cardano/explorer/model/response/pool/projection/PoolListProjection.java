package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;


public interface PoolListProjection {

  Long getPoolId();

  String getPoolView();

  String getPoolName();

  BigInteger getPledge();

  BigInteger getFee();

  Integer getParamK();

  Double getMargin();

  BigInteger getReserves();
}
