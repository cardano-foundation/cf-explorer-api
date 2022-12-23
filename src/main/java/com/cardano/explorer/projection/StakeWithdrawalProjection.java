package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface StakeWithdrawalProjection {

  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getAmount();
}
