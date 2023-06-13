package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface PoolInfoKoiosProjection {

  String getView();

  BigInteger getActiveStake();

  Double getSaturation();
}
