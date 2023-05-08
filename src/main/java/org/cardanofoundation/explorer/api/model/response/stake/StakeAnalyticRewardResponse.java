package org.cardanofoundation.explorer.api.model.response.stake;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigInteger;

@Getter
@AllArgsConstructor
public class StakeAnalyticRewardResponse {
  private Integer epoch;
  private BigInteger value;
}
