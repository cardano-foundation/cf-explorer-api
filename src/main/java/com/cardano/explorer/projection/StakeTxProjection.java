package com.cardano.explorer.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface StakeTxProjection {
  Long getTxId();
  BigInteger getAmount();

  Timestamp getTime();

}
