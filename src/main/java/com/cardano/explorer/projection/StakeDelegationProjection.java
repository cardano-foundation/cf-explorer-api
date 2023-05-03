package com.cardano.explorer.projection;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.math.BigInteger;
import java.sql.Timestamp;

@JsonInclude(Include.NON_NULL)
public interface StakeDelegationProjection {
  String getStakeAddress();
  String getTxHash();
  BigInteger getOutSum();
  BigInteger getFee();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getPoolId();
  String getTickerName();
  String getPoolData();
}
