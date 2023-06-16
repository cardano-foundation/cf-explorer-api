package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface InstantaneousRewardsProjection {
  Long getTxId();
  Long getNumberOfStakes();
  BigInteger getRewards();
}
