package com.cardano.explorer.model.response.pool.lifecycle;

import java.math.BigInteger;
import java.sql.Timestamp;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class RewardResponse {

  private Integer epochNo;

  private Timestamp time;

  private BigInteger amount;

  private String rewardAccount;
}
