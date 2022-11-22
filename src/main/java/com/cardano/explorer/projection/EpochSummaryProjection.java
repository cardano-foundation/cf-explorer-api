package com.cardano.explorer.projection;

import java.sql.Timestamp;

public interface EpochSummaryProjection {
  Integer getNo();
  Integer getMaxSlot();
  Timestamp getStartTime();
}
