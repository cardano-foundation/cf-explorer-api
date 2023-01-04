package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.json.serialize.PercentSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
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

  private BigDecimal poolSize;

  private BigDecimal stakeLimit;

  private Integer delegators;

  @JsonSerialize(using = PercentSerializer.class)
  private Double saturation;

  @JsonSerialize(using = PercentSerializer.class)
  private Double reward;

  @JsonSerialize(using = PercentSerializer.class)
  private Double ros;

  private BigDecimal pledge;

  private BigDecimal cost;

  @JsonSerialize(using = PercentSerializer.class)
  private Double margin;

  private Integer epochBlock;

  private Integer lifetimeBlock;
}
