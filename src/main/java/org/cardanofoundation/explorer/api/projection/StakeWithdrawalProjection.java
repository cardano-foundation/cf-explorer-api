package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface StakeWithdrawalProjection {

  Long getTxId();
  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  BigInteger getAmount();
  BigInteger getFee();
}
