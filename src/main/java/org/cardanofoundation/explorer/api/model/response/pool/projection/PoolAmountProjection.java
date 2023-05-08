package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface PoolAmountProjection {

  Long getPoolId();

  BigInteger getAmount();
}
