package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface StakeDelegationProjection {

  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getPoolId();
  String getPoolData();
}
