package com.cardano.explorer.model.request.pool;

import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.cardano.explorer.model.response.pool.projection.PoolListProjection;
import com.cardano.explorer.model.response.pool.projection.RewardEpochProjection;
import com.cardano.explorer.projection.PoolDelegationSummaryProjection;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RewardParam {

  private BigDecimal currentAda;

  private Double expansionRate;

  private BigDecimal feePerEpoch;

  private Double treasuryRate;

  private Integer k;

  private BigDecimal pledge;

  private Double a0;

  private BigDecimal poolSize;

  private Integer blkCount;

  private Integer maxBlockSize;

  private Double margin;

  private BigDecimal fixedFee;

  public RewardParam(PoolListProjection param) {
    this.currentAda = param.getUtxo();
    this.expansionRate = param.getExpansionRate();
    this.feePerEpoch = param.getFeePerEpoch();
    this.treasuryRate = param.getTreasuryRate();
    this.k = param.getParamK();
    this.pledge = param.getPledge();
    this.a0 = param.getInfluence();
    this.poolSize = param.getPoolSize();
    this.blkCount = param.getBlkCount();
    this.maxBlockSize = param.getMaxBlockSize();
    this.margin = param.getMargin();
    this.fixedFee = param.getFee();
  }

  public RewardParam(PoolDelegationSummaryProjection param) {
    this.currentAda = param.getUtxo();
    this.expansionRate = param.getExpansionRate();
    this.feePerEpoch = param.getFeePerEpoch();
    this.treasuryRate = param.getTreasuryRate();
    this.k = param.getOptimalPoolCount();
    this.pledge = param.getPledge();
    this.a0 = param.getInfluence();
    this.poolSize = param.getPoolSize();
    this.blkCount = param.getBlkCount();
    this.maxBlockSize = param.getMaxBlockSize();
    this.margin = param.getMargin();
    this.fixedFee = param.getFee();
  }

  public RewardParam(PoolDetailUpdateProjection param) {
    this.currentAda = param.getUtxo();
    this.expansionRate = param.getExpansionRate();
    this.feePerEpoch = param.getFeePerEpoch();
    this.treasuryRate = param.getTreasuryRate();
    this.k = param.getParamK();
    this.pledge = param.getPledge();
    this.a0 = param.getInfluence();
    this.poolSize = param.getPoolSize();
    this.blkCount = param.getBlkCount();
    this.maxBlockSize = param.getMaxBlockSize();
    this.margin = param.getMargin();
    this.fixedFee = param.getCost();
  }

  public RewardParam(RewardEpochProjection param) {
    this.currentAda = param.getUtxo();
    this.expansionRate = param.getExpansionRate();
    this.feePerEpoch = param.getFeePerEpoch();
    this.treasuryRate = param.getTreasuryRate();
    this.k = param.getParamK();
    this.a0 = param.getInfluence();
    this.blkCount = param.getBlkCount();
    this.maxBlockSize = param.getMaxBlockSize();
  }
}
