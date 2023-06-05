package org.cardanofoundation.explorer.api.model.response.dashboard;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class EpochSummary {
  protected Integer no;
  private Integer slot;
  private Integer totalSlot;
  private Integer account;
}
