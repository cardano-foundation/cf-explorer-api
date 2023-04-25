package com.cardano.explorer.model.response.pool.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface PoolRegistrationProjection {

  BigInteger getPledge();

  Double getMargin();

  String getVrfKey();

  BigInteger getCost();

  String getTxHash();

  Timestamp getTime();

  BigInteger getDeposit();

  BigInteger getFee();

  String getRewardAccount();
}
