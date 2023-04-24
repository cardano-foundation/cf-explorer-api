package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

public interface StakeInstantaneousRewardsProjection {

  String getTxHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getBlockIndex();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  String getAmount();
}
