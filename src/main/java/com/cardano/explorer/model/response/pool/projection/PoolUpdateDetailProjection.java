package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolUpdateDetailProjection {

  Long getHashId();

  String getPoolId();

  String getPoolName();

  String getPoolView();

  String getTxHash();

  Timestamp getTime();

  BigInteger getFee();

  String getRewardAccount();

  String getVrfKey();

  BigInteger getPledge();

  Double getMargin();

  BigInteger getCost();
}
