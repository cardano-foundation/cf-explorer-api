package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface StakeRewardProjection {
  BigInteger getAmount();

  Long getStakeAddressId();
}
