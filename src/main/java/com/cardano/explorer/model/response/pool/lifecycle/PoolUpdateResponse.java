package com.cardano.explorer.model.response.pool.lifecycle;

import com.cardano.explorer.model.response.pool.projection.PoolUpdateProjection;
import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolUpdateResponse implements Serializable {

  private Long poolUpdateId;

  private String txHash;

  private BigInteger fee;

  private Timestamp time;

  public PoolUpdateResponse(PoolUpdateProjection projection) {
    this.poolUpdateId = projection.getPoolUpdateId();
    this.txHash = projection.getTxHash();
    this.fee = projection.getFee();
    this.time = projection.getTime();
  }
}
