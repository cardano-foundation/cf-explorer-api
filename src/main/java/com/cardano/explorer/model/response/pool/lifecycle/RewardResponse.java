package com.cardano.explorer.model.response.pool.lifecycle;

import com.cardano.explorer.model.response.pool.projection.LifeCycleRewardProjection;
import java.math.BigInteger;
import java.sql.Timestamp;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class RewardResponse {

  private Integer epochNo;

  private Timestamp time;

  private BigInteger amount;

  private String rewardAccount;

  public RewardResponse(LifeCycleRewardProjection projection) {
    this.epochNo = projection.getEpochNo();
    this.time = projection.getTime();
    this.amount = projection.getAmount();
    this.rewardAccount = projection.getAddress();
  }
}
