package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;

public interface PoolDetailUpdateProjection {

  BigDecimal getCost();

  Double getMargin();

  BigDecimal getPledge();

  Integer getParamK();

  BigDecimal getUtxo();
}
