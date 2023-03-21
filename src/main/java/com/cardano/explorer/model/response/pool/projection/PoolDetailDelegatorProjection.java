package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolDetailDelegatorProjection {

  Long getStakeAddressId();

  Timestamp getTime();

  BigInteger getFee();

  String getView();
}
