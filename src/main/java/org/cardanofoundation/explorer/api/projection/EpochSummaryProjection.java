package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

public interface EpochSummaryProjection {
  Integer getNo();
  Integer getMaxSlot();
  Timestamp getStartTime();

  Timestamp getEndTime();
}
