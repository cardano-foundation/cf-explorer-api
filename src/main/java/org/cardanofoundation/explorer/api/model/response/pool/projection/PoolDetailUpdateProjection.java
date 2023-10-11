package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

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

  String getJson();

  String getLogoUrl();

  String getIconUrl();
}
