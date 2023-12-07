package org.cardanofoundation.explorer.api.projection;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.sql.Timestamp;

@Getter
@AllArgsConstructor
public class EpochTimeProjection {
  Integer epochNo;
  Timestamp startTime;
  Timestamp endTime;
}
