package com.cardano.explorer.model.response.pool.projection;

import java.math.BigDecimal;
import java.sql.Timestamp;

public interface TxBlockEpochProjection {

  Long getTxId();

  String getTxHash();

  Timestamp getTxTime();

  Integer getEpochNo();

  Long getSlotNo();

  BigDecimal getPledge();

  Double getMargin();

  BigDecimal getCost();

  Long getPoolId();

  String getPoolName();

  Long getBlockId();
}
