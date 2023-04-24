package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface StakeAddressProjection {

  Long getAddress();

  String getStakeAddress();

  BigInteger getTotalStake();
}
