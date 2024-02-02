package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

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

  Integer getSlotNo();

  String getPoolId();

  String getPoolName();

  String getTickerName();

  String getPoolData();

  String getLogoUrl();

  String getIconUrl();
}
