package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface StakeDelegationProjection {
  String getStakeAddress();
  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getPoolId();
  String getTickerName();
  String getPoolData();
}
