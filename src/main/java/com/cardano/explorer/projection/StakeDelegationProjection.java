package com.cardano.explorer.projection;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.sql.Timestamp;

@JsonInclude(Include.NON_NULL)
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
