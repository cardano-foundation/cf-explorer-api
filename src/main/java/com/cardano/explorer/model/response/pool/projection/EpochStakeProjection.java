package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface EpochStakeProjection {

  Integer getEpochNo();

  BigDecimal getTotalStake();
}
