package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;

public interface EpochRewardProjection {

  Integer getEpochNo();

  BigInteger getAmount();
}
