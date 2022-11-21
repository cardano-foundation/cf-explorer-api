package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Page;

@Getter
@Setter
public class PoolDetailResponse implements Serializable {

  private String poolName;

  private String tickerName;

  private String poolView;

  private Timestamp createDate;

  private String rewardAccount;

  private String ownerAccount;

  private BigDecimal poolSize;

  private BigDecimal stakeLimit;

  private Integer delegators;

  private Double saturation;

  private Double reward;

  private Double fee;

  private Double ros;

  private BigDecimal pledge;

  private BigDecimal cost;

  private Double margin;

  private Integer epochBlock;

  private Integer lifetimeBlock;

  private Page<PoolDetailEpochResponse> epoch;
}
