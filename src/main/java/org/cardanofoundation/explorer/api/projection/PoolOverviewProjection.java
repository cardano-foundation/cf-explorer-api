package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface PoolOverviewProjection {
  String getPoolHash();

  Long getCreatedAt();

  Long getPoolId();

  BigInteger getBalance();
}
