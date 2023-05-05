package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface RewardEpochProjection {

  Integer getEpochNo();

  Integer getParamK();

  BigInteger getUtxo();

  BigInteger getFeePerEpoch();

  Double getInfluence();

  Double getExpansionRate();

  Double getTreasuryRate();

  Integer getBlkCount();

  Integer getMaxBlockSize();
}
