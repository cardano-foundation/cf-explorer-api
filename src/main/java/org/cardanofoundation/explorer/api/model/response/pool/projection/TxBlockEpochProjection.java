package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface TxBlockEpochProjection {

  Long getTxId();

  String getTxHash();

  Timestamp getTxTime();

  Integer getEpochNo();

  Integer getSlotNo();

  BigInteger getPledge();

  Double getMargin();

  BigInteger getCost();

  Long getPoolId();

  String getPoolName();

  Long getBlockNo();

  String getPoolView();
}
