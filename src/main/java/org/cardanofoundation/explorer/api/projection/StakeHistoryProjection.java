package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

public interface StakeHistoryProjection {

  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getBlockIndex();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  Integer getSlotNo();
  String getAction();
  BigInteger getFee();
  Long getDeposit();
}
