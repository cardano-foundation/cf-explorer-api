package org.cardanofoundation.explorer.api.model.response.dashboard;

import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EpochSummary {
  protected Integer no;
  private Integer slot;
  private Integer totalSlot;
  private Integer account;
  private LocalDateTime startTime;
  private LocalDateTime endTime;
}
