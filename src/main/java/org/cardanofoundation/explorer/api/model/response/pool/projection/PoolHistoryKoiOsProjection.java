package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface PoolHistoryKoiOsProjection {

  String getView();

  BigInteger getDelegateReward();

  Double getRos();

  BigInteger getActiveStake();

  BigInteger getPoolFees();

  Integer getEpochNo();
}
