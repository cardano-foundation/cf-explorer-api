package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;

public interface RewardRefundProjection {

  Integer getEpochNo();

  BigInteger getAmount();
}
