package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface PoolRangeProjection {
  BigInteger getMaxPoolSize();

  BigInteger getMinPoolSize();

  BigInteger getMaxPledge();

  BigInteger getMinPledge();

  Double getMaxSaturation();

  Double getMinSaturation();

  Integer getMaxLifetimeBlock();

  Integer getMinLifetimeBlock();

  Double getMaxVotingPower();

  Double getMinVotingPower();

  Double getMaxGovParticipationRate();

  Double getMinGovParticipationRate();
}
