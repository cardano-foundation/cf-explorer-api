package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;

public interface EpochRewardProjection {

  Integer getEpochNo();

  BigInteger getAmount();
}
