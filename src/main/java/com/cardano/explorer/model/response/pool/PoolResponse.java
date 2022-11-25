package com.cardano.explorer.model.response.pool;

import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolResponse implements Serializable {

  private Long poolId;

  private String poolName;

  private BigDecimal poolSize;

  private Double reward;

  private Double feePercent;

  private BigDecimal feeAmount;

  private BigDecimal pledge;

  private Double saturation;

  public PoolResponse(Long poolId) {
    this.poolId = poolId;
  }
}
