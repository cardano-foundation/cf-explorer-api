package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface RewardEpochProjection {

  Integer getEpochNo();

  Integer getParamK();

  BigDecimal getUtxo();

  BigDecimal getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();
}
