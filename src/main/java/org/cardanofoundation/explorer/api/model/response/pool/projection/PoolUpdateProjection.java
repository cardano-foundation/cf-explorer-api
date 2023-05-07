package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolUpdateProjection {

   Long getPoolUpdateId();

   String getTxHash();

   BigInteger getFee();

   Timestamp getTime();

   Double getMargin();
}
