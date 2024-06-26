package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;

@Getter
@Setter
public class DeRegistrationResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private List<String> stakeKeys;

  private String txHash;

  private BigInteger totalFee;

  private BigInteger poolHold;

  private Timestamp time;

  private BigInteger fee;

  private Integer retiringEpoch;

  private boolean refundFlag;

  public DeRegistrationResponse(PoolDeRegistrationProjection projection) {
    this.txHash = projection.getTxHash();
    this.time = projection.getTime();
    this.fee = projection.getFee();
    this.retiringEpoch = projection.getRetiringEpoch();
    this.refundFlag = projection.getRefundFlag();
  }
}
