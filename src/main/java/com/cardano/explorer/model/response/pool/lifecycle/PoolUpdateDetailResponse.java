package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PoolUpdateDetailResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private BigInteger previousPledge;

  private Double previousMargin;

  private String txHash;

  private BigInteger totalFees;

  private Timestamp time;

  private List<String> stakeKeys;

  private BigInteger fees;

  private List<String> rewardAccounts;

  private String vrfKey;

  private BigInteger pledge;

  private Double margin;

  private BigInteger cost;
}
