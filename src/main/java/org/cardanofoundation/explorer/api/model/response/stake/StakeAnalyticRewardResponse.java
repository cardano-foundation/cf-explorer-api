package org.cardanofoundation.explorer.api.model.response.stake;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class StakeAnalyticRewardResponse {
  private Integer epoch;
  private BigInteger value;
}
