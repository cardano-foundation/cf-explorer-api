package org.cardanofoundation.explorer.api.model.response.pool;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import lombok.*;
import org.cardanofoundation.explorer.api.json.serialize.PercentSerializer;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
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

  private BigInteger pledge;

  private BigInteger cost;

  @JsonSerialize(using = PercentSerializer.class)
  private Double margin;

  private Integer epochBlock;

  private Integer lifetimeBlock;

  private String homepage;

  private String description;

  private String logoUrl;

  private String iconUrl;

  private BigInteger totalBalanceOfPoolOwners;

  private Timestamp lastUpdate;

  public PoolDetailHeaderResponse(PoolDetailUpdateProjection poolDetail) {
    this.poolName = poolDetail.getPoolName();
    this.tickerName = poolDetail.getTickerName();
    this.hashView = poolDetail.getHashRaw();
    this.pledge = poolDetail.getPledge();
    this.cost = poolDetail.getCost();
    this.margin = poolDetail.getMargin();
    this.epochBlock = poolDetail.getEpochBlock();
    this.lifetimeBlock = poolDetail.getLifetimeBlock();
    this.delegators = poolDetail.getDelegators();
    this.lastUpdate = poolDetail.getLastUpdate();
    this.rewardAccounts = Collections.singletonList(poolDetail.getRewardAddress());
    this.logoUrl = poolDetail.getLogoUrl();
    this.iconUrl = poolDetail.getIconUrl();
  }
}
