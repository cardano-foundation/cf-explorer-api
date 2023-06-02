package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface PoolInfoKoiOsProjection {

  String getView();

  BigInteger getActiveStake();

  Double getSaturation();
}
