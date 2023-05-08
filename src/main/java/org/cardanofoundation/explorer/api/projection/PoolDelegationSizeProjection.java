package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface PoolDelegationSizeProjection {
  BigInteger getPoolSize();
  Long getPoolId();
}
