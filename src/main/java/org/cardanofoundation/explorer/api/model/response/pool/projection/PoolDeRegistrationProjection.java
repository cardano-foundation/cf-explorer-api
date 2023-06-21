package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolDeRegistrationProjection {

  Boolean getRefundFlag();

  BigInteger getFee();

  String getTxHash();

  Long getTxId();

  String getPoolId();

  Integer getRetiringEpoch();

  Timestamp getTime();
}
