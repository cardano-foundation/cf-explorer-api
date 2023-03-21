package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface PoolDelegationSizeProjection {
  BigDecimal getPoolSize();
  Long getPoolId();
}
