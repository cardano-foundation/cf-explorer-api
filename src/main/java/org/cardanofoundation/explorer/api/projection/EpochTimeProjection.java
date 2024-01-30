package org.cardanofoundation.explorer.api.projection;

import java.sql.Timestamp;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class EpochTimeProjection {
  Integer epochNo;
  Timestamp startTime;
  Timestamp endTime;
}
