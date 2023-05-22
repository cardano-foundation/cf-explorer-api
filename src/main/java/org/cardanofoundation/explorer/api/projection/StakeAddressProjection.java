package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface StakeAddressProjection {
  Long getId();

  Long getAddress();

  String getStakeAddress();

  BigInteger getTotalStake();
}
