package org.cardanofoundation.explorer.api.model.response.dashboard;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class EpochSummary {
  protected Integer no;
  private Integer slot;
  private Integer totalSlot;
  private Integer account;
  private LocalDateTime startTime;
  private LocalDateTime endTime;
  private BigInteger circulatingSupply;
}
