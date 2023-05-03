package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;


public interface PoolListProjection {

  String getPoolView();

  String getPoolName();

  BigInteger getPledge();

  BigInteger getFee();

  BigInteger getPoolSize();

  Integer getParamK();

  BigInteger getUtxo();

  Double getMargin();

  BigInteger getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();
}
