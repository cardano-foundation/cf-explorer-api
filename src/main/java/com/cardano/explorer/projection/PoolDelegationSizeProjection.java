package com.cardano.explorer.projection;

import java.math.BigInteger;

public interface PoolDelegationSizeProjection {
  BigInteger getPoolSize();
  Long getPoolId();
}
