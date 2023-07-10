package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface PoolHistoryKoiosProjection {

  String getView();

  BigInteger getDelegateReward();

  Double getRos();

  BigInteger getActiveStake();

  BigInteger getPoolFees();

  Integer getEpochNo();

  BigDecimal getActiveStakePct();
}
