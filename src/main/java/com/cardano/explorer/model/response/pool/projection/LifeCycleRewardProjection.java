package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface LifeCycleRewardProjection {

  Integer getEpochNo();

  Timestamp getTime();

  BigInteger getAmount();

  String getAddress();
}
