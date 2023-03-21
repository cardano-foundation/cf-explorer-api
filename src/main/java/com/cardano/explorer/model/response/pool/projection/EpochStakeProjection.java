package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;

public interface EpochStakeProjection {

  Integer getEpochNo();

  BigInteger getTotalStake();
}
