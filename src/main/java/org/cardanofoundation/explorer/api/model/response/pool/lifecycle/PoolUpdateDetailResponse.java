package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;

@Getter
@Setter
@NoArgsConstructor
public class PoolUpdateDetailResponse implements Serializable {

  private Long poolUpdateId;

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

  private BigInteger deposit;

  public PoolUpdateDetailResponse(PoolUpdateDetailProjection projection) {
    this.poolUpdateId = projection.getPoolUpdateId();
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
    this.deposit = projection.getDeposit();
  }
}
