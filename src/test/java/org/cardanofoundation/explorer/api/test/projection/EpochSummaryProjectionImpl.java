package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;

import lombok.Builder;

import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;

@Builder
public class EpochSummaryProjectionImpl implements EpochSummaryProjection {

  private Integer no;
  private Integer maxSlot;
  private Timestamp statTime;

  @Override
  public Integer getNo() {
    return no;
  }

  @Override
  public Integer getMaxSlot() {
    return maxSlot;
  }

  @Override
  public Timestamp getStartTime() {
    return statTime;
  }
}
