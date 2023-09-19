package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;
import lombok.Builder;
import org.cardanofoundation.explorer.api.projection.EpochSummaryProjection;

@Builder
public class EpochSummaryProjectionImpl implements EpochSummaryProjection {

  private Integer no;
  private Integer maxSlot;
  private Timestamp statTime;
  private Timestamp endTime;
  private Integer blkCount;

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

  @Override
  public Timestamp getEndTime() {
    return endTime;
  }

  @Override
  public Integer getBlkCount() {
    return blkCount;
  }
}
