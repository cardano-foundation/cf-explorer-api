package com.cardano.explorer.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.util.Date;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class StakeRewardResponse {
  private Integer epoch;
  private Date time;
  private BigInteger amount;

}
