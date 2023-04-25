package com.cardano.explorer.model.response.pool.lifecycle;

import com.cardano.explorer.model.response.pool.projection.PoolUpdateDetailProjection;
import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PoolUpdateDetailResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private BigInteger previousPledge;

  private Double previousMargin;

  private String txHash;

  private Timestamp time;

  private List<String> stakeKeys;

  private BigInteger fee;

  private String rewardAccount;

  private String vrfKey;

  private BigInteger pledge;

  private Double margin;

  private BigInteger cost;

  public PoolUpdateDetailResponse(PoolUpdateDetailProjection projection) {
    this.poolId = projection.getPoolId();
    this.poolName = projection.getPoolName();
    this.poolView = projection.getPoolView();
    this.txHash = projection.getTxHash();
    this.time = projection.getTime();
    this.fee = projection.getFee();
    this.rewardAccount = projection.getRewardAccount();
    this.vrfKey = projection.getVrfKey();
    this.pledge = projection.getPledge();
    this.margin = projection.getMargin();
    this.cost = projection.getCost();
  }
}
