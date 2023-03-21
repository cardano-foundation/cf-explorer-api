package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface EpochSummary {
  Integer getNo();
  Integer getMaxSlot();
  Timestamp getStartTime();
}
