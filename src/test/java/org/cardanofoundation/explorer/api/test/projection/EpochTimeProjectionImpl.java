package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;

import lombok.Builder;

import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;

@Builder
public class EpochTimeProjectionImpl implements EpochTimeProjection {

  private Timestamp startTime;
  private Timestamp endTime;
  private Integer epochNo;

  @Override
  public Integer getEpochNo() {
    return epochNo;
  }

  @Override
  public Timestamp getStartTime() {
    return startTime;
  }

  @Override
  public Timestamp getEndTime() {
    return endTime;
  }
}
