package org.cardanofoundation.explorer.api.model.response.drep;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DRepOverviewResponse {

  private Integer epochNo;
  private Long countDownEndTime;

  private Integer epochSlotNo;

  private BigInteger activeStake;
  private Long delegators;

  private Long totalDReps;
  private Long activeDReps;
  private Long inactiveDReps;
  private Long retiredDReps;

  private Long abstainDReps;
  private Long noConfidenceDReps;
  private Long registeredDReps;
  private BigInteger totalAdaStaked;
}
