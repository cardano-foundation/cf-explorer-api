package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.model.response.pool.custom.PoolDetailEpoch;
import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailEpochResponse implements Serializable {

  private Integer epoch;

  private Long block;

  private BigDecimal stakeAmount;

  private BigDecimal delegators;

  private BigDecimal fee;

  private Double ros;

  public PoolDetailEpochResponse(PoolDetailEpoch poolEpoch) {
    this.epoch = poolEpoch.getEpochNo();
    this.block = poolEpoch.getCountBlock();
  }
}
