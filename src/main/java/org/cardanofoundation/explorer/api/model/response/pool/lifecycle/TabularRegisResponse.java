package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRegistrationProjection;
import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TabularRegisResponse implements Serializable {

  private Long poolUpdateId;

  private String txHash;

  private BigInteger totalFee;

  private Timestamp time;

  private BigInteger fee;

  private List<String> stakeKeys;

  private BigInteger deposit;

  public TabularRegisResponse(PoolRegistrationProjection projection) {
    this.poolUpdateId = projection.getPoolUpdateId();
    this.txHash = projection.getTxHash();
    this.totalFee = projection.getDeposit().add(projection.getFee());
    this.time = projection.getTime();
    this.fee = projection.getFee();
    this.deposit = projection.getDeposit();
  }
}
