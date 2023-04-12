package com.cardano.explorer.model.response.stake;

import java.math.BigInteger;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class StakeAnalyticRewardResponse {
  private Long epoch;
  private BigInteger value;
}
