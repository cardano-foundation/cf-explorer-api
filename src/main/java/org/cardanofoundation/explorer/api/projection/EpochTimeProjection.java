package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

public interface EpochTimeProjection {
  Integer getEpochNo();
  Timestamp getStartTime();
  Timestamp getEndTime();
}
