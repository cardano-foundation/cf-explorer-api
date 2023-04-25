package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.json.serialize.PercentSerializer;
import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailHeaderResponse implements Serializable {

  private String poolName;

  private String tickerName;

  private String hashView;

  private Timestamp createDate;

  private List<String> rewardAccounts;

  private List<String> ownerAccounts;

  private BigInteger poolSize;

  private BigDecimal stakeLimit;

  private Integer delegators;

  @JsonSerialize(using = PercentSerializer.class)
  private Double saturation;

  @JsonSerialize(using = PercentSerializer.class)
  private Double reward;

  @JsonSerialize(using = PercentSerializer.class)
  private Double ros;

  private BigInteger pledge;

  private BigInteger cost;

  @JsonSerialize(using = PercentSerializer.class)
  private Double margin;

  private Integer epochBlock;

  private Integer lifetimeBlock;

  public PoolDetailHeaderResponse(PoolDetailUpdateProjection poolDetail) {
    this.poolName = poolDetail.getPoolName();
    this.tickerName = poolDetail.getTickerName();
    this.hashView = poolDetail.getHashRaw();
    this.pledge = poolDetail.getPledge();
    this.cost = poolDetail.getCost();
    this.margin = poolDetail.getMargin();
    this.rewardAccounts = Collections.singletonList(poolDetail.getRewardAddress());
  }
}
