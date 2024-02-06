package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;

import lombok.Builder;

@Builder
public class EpochTimeProjectionImpl {

  private Timestamp startTime;
  private Timestamp endTime;
  private Integer epochNo;
}
