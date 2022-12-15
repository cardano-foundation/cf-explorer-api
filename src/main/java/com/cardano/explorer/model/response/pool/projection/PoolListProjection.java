package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface PoolListProjection {

  String getPoolView();

  String getPoolName();

  BigDecimal getPledge();

  BigDecimal getFee();

  BigDecimal getPoolSize();

  Integer getParamK();
}
