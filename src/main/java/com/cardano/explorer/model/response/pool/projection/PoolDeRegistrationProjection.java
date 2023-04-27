package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolDeRegistrationProjection {

  BigInteger getFee();

  String getTxHash();

  Long getTxId();

  String getPoolId();

  Integer getRetiringEpoch();

  Timestamp getTime();
}
