package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
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
public class RegistrationResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private String txHash;

  private BigInteger totalFee;

  private Timestamp time;

  private BigInteger fee;

  private String rewardAccount;

  private List<String> stakeKeys;

  private String vrfKey;

  private BigInteger pledge;

  private Double margin;

  private BigInteger cost;

  private BigInteger deposit;

  public RegistrationResponse(PoolRegistrationProjection projection) {
    this.txHash = projection.getTxHash();
    this.totalFee = projection.getDeposit().add(projection.getFee());
    this.time = projection.getTime();
    this.fee = projection.getFee();
    this.vrfKey = projection.getVrfKey();
    this.pledge = projection.getPledge();
    this.margin = projection.getMargin();
    this.cost = projection.getCost();
    this.deposit = projection.getDeposit();
    this.rewardAccount = projection.getRewardAccount();
  }
}
