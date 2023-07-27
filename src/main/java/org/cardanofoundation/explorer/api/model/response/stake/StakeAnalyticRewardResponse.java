package org.cardanofoundation.explorer.api.model.response.stake;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigInteger;

@Getter
@AllArgsConstructor
@Builder
@NoArgsConstructor
public class StakeAnalyticRewardResponse {
  private Integer epoch;
  private BigInteger value;
}
