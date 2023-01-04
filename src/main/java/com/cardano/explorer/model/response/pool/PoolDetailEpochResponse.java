package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.json.serialize.PercentSerializer;
import com.cardano.explorer.model.response.pool.projection.PoolDetailEpochProjection;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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

  @JsonSerialize(using = PercentSerializer.class)
  private Double ros;

  public PoolDetailEpochResponse(PoolDetailEpochProjection poolEpoch) {
    this.epoch = poolEpoch.getEpochNo();
    this.block = poolEpoch.getCountBlock();
  }
}
