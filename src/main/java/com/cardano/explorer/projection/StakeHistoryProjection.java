package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface StakeHistoryProjection {

  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getAction();
}
