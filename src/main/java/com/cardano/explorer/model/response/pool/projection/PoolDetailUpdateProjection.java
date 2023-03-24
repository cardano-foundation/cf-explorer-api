package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface PoolDetailUpdateProjection {

  Long getPoolId();

  String getHashRaw();

  BigInteger getPoolSize();

  String getPoolName();

  String getTickerName();

  BigInteger getCost();

  Double getMargin();

  BigInteger getPledge();

  Integer getParamK();

  BigInteger getUtxo();

  BigInteger getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();

  BigInteger getReserves();
}
