package com.cardano.explorer.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface PoolDelegationSummaryProjection {

  String getPoolView();

  String getJson();

  BigInteger getPledge();

  BigInteger getFee();

  BigInteger getPoolSize();

  Integer getOptimalPoolCount();

  BigInteger getUtxo();

  Double getMargin();

  BigInteger getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();

  BigInteger getReserves();
}
