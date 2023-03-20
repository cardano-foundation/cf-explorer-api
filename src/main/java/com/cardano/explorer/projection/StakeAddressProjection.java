package com.cardano.explorer.projection;

import java.math.BigInteger;

public interface StakeAddressProjection {

  Long getAddress();

  String getStakeAddress();

  BigInteger getTotalStake();
}
