package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigDecimal;

public interface PoolDetailUpdateProjection {

  Long getPoolId();

  String getHashRaw();

  BigDecimal getPoolSize();

  String getPoolName();

  String getTickerName();

  BigDecimal getCost();

  Double getMargin();

  BigDecimal getPledge();

  Integer getParamK();

  BigDecimal getUtxo();

  BigDecimal getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();
}
