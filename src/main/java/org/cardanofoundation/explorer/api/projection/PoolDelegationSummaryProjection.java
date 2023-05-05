package org.cardanofoundation.explorer.api.projection;

import java.math.BigDecimal;

public interface PoolDelegationSummaryProjection {

  String getPoolView();

  String getJson();

  BigDecimal getPledge();

  BigDecimal getFee();

  BigDecimal getPoolSize();

  Integer getOptimalPoolCount();

  BigDecimal getUtxo();

  Double getMargin();

  BigDecimal getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();
}
