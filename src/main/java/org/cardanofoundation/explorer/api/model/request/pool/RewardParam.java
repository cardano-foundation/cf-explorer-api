package org.cardanofoundation.explorer.api.model.request.pool;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.RewardEpochProjection;
import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RewardParam {

  private BigInteger currentAda;
  private Double expansionRate;

  private BigInteger feePerEpoch;

  private Double treasuryRate;

  private Integer k;

  private BigInteger pledge;

  private Double a0;

  private BigInteger poolSize;

  private Integer blkCount;

  private Integer maxBlockSize;

  private Double margin;

  private BigInteger fixedFee;

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
