package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

public interface EpochSummary {
  Integer getNo();
  Integer getMaxSlot();
  Timestamp getStartTime();
}
