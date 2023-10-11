package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolDetailUpdateProjection {

  Long getPoolId();

  String getPoolView();

  String getHashRaw();

  String getPoolName();

  String getTickerName();

  BigInteger getCost();

  Double getMargin();

  BigInteger getPledge();

  Integer getParamK();

  BigInteger getReserves();

  String getRewardAddress();

  Integer getEpochBlock();
  Integer getLifetimeBlock();
  Integer getDelegators();
  Timestamp getLastUpdate();

  String getJson();

  String getLogoUrl();

  String getIconUrl();

}
