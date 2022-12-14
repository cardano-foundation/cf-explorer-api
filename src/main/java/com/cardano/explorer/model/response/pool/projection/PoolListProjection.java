package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface PoolListProjection {

  Long getPoolId();

  String getPoolName();

  BigDecimal getPledge();

  BigDecimal getFee();

  BigDecimal getPoolSize();

  Integer getParamK();
}
