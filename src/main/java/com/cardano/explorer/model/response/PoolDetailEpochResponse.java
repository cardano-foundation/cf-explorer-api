package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailEpochResponse implements Serializable {

  private Integer epoch;

  private Integer block;

  private BigDecimal stakeAmount;

  private BigDecimal delegators;

  private BigDecimal fee;

  private Double ros;
}
