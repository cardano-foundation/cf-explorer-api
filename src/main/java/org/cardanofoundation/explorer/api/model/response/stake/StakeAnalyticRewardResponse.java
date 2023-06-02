package org.cardanofoundation.explorer.api.model.response.stake;

import java.math.BigInteger;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class StakeAnalyticRewardResponse {
  private Integer epoch;
  private BigInteger value;
}
