package org.cardanofoundation.explorer.api.test.projection;

import org.cardanofoundation.explorer.api.projection.ParamHistory;
import java.math.BigInteger;
import java.util.Date;
import lombok.Builder;

@Builder
public class ParamHistoryProjection implements ParamHistory {

  private Long id;
  private BigInteger minFeeA;
  private Long tx;
  private BigInteger minFeeB;
  private BigInteger maxBlockSize;
  private BigInteger maxTxSize;
  private BigInteger maxBhSize;
  private BigInteger keyDeposit;
  private BigInteger poolDeposit;
  private BigInteger maxEpoch;
  private BigInteger optimalPoolCount;
  private BigInteger maxTxExMem;
  private BigInteger maxTxExSteps;
  private Double influence;
  private BigInteger maxBlockExMem;
  private BigInteger maxBlockExSteps;
  private BigInteger maxValSize;
  private BigInteger coinsPerUtxoSize;
  private Double monetaryExpandRate;
  private Double treasuryGrowthRate;
  private Double decentralisation;
  private Double priceMem;
  private Double priceStep;
  private Integer protocolMajor;
  private Integer protocolMinor;
  private Integer collateralPercent;
  private Integer maxCollateralInputs;
  private String extraEntropy;
  private Long costModel;
  private Date time;
  private String hash;
  private BigInteger minUtxoValue;
  private BigInteger minPoolCost;

  @Override
  public Long getId() {
    return this.id;
  }

  @Override
  public BigInteger getMinFeeA() {
    return this.minFeeA;
  }

  @Override
  public BigInteger getMinFeeB() {
    return this.minFeeB;
  }

  @Override
  public BigInteger getMaxBlockSize() {
    return this.maxBlockSize;
  }

  @Override
  public BigInteger getMaxTxSize() {
    return this.maxTxSize;
  }

  @Override
  public BigInteger getMaxBhSize() {
    return this.maxBhSize;
  }

  @Override
  public BigInteger getKeyDeposit() {
    return this.keyDeposit;
  }

  @Override
  public BigInteger getPoolDeposit() {
    return this.poolDeposit;
  }

  @Override
  public BigInteger getMaxEpoch() {
    return this.maxEpoch;
  }

  @Override
  public BigInteger getOptimalPoolCount() {
    return this.optimalPoolCount;
  }

  @Override
  public BigInteger getMinUtxoValue() {
    return this.minUtxoValue;
  }

  @Override
  public BigInteger getMinPoolCost() {
    return this.minPoolCost;
  }

  @Override
  public BigInteger getMaxTxExMem() {
    return this.maxTxExMem;
  }

  @Override
  public BigInteger getMaxTxExSteps() {
    return this.maxTxExSteps;
  }

  @Override
  public BigInteger getMaxBlockExMem() {
    return this.maxBlockExMem;
  }

  @Override
  public BigInteger getMaxBlockExSteps() {
    return this.maxBlockExSteps;
  }

  @Override
  public BigInteger getMaxValSize() {
    return this.maxValSize;
  }

  @Override
  public BigInteger getCoinsPerUtxoSize() {
    return this.coinsPerUtxoSize;
  }

  @Override
  public Double getInfluence() {
    return this.influence;
  }

  @Override
  public Double getMonetaryExpandRate() {
    return this.monetaryExpandRate;
  }

  @Override
  public Double getTreasuryGrowthRate() {
    return this.treasuryGrowthRate;
  }

  @Override
  public Double getDecentralisation() {
    return this.decentralisation;
  }

  @Override
  public Double getPriceMem() {
    return

        this.priceMem;
  }

  @Override
  public Double getPriceStep() {
    return this.priceStep;
  }

  @Override
  public Integer getProtocolMajor() {
    return this.protocolMajor;
  }

  @Override
  public Integer getProtocolMinor() {
    return this.protocolMinor;
  }

  @Override
  public Integer getCollateralPercent() {
    return this.collateralPercent;
  }

  @Override
  public Integer getMaxCollateralInputs() {
    return this.maxCollateralInputs;
  }

  @Override
  public String getEntropy() {
    return
        this.extraEntropy;
  }

  @Override
  public Long getCostModel() {
    return this.costModel;
  }

  @Override
  public Long getTx() {
    return tx;
  }

  @Override
  public Date getTime() {
    return this.time;
  }

  @Override
  public String getHash() {
    return this.hash;
  }
}
