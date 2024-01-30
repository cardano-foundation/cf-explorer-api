package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateProjection;

@Getter
@Setter
public class PoolUpdateResponse implements Serializable {

  private Long poolUpdateId;

  private String txHash;

  private BigInteger fee;

  private Timestamp time;

  private Double margin;

  private BigInteger poolHold;

  public PoolUpdateResponse(PoolUpdateProjection projection) {
    this.poolUpdateId = projection.getPoolUpdateId();
    this.txHash = projection.getTxHash();
    this.fee = projection.getFee();
    this.time = projection.getTime();
    this.margin = projection.getMargin();
    this.poolHold = projection.getDeposit();
  }
}
