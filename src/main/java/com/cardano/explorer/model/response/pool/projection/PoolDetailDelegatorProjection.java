package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;
import java.sql.Timestamp;

public interface PoolDetailDelegatorProjection {

  Long getStakeAddressId();

  Timestamp getTime();

  BigDecimal getFee();

  String getView();
}
