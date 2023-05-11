package org.cardanofoundation.explorer.api.model.response.protocol;

import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Protocols {

  EpochChange epochChange;
  ProtocolHistory minFeeA;
  ProtocolHistory minFeeB;
  ProtocolHistory maxBlockSize;
  ProtocolHistory maxTxSize;
  ProtocolHistory maxBhSize;
  ProtocolHistory keyDeposit;
  ProtocolHistory poolDeposit;
  ProtocolHistory maxEpoch;
  ProtocolHistory optimalPoolCount;
  ProtocolHistory influence;
  ProtocolHistory monetaryExpandRate;
  ProtocolHistory treasuryGrowthRate;
  ProtocolHistory decentralisation;
  ProtocolHistory entropy;
  ProtocolHistory protocolMajor;
  ProtocolHistory protocolMinor;
  ProtocolHistory minUtxoValue;
  ProtocolHistory minPoolCost;
  ProtocolHistory costModel;
  ProtocolHistory priceMem;
  ProtocolHistory priceStep;
  ProtocolHistory maxTxExMem;
  ProtocolHistory maxTxExSteps;
  ProtocolHistory maxBlockExMem;
  ProtocolHistory maxBlockExSteps;
  ProtocolHistory maxValSize;
  ProtocolHistory collateralPercent;
  ProtocolHistory maxCollateralInputs;
  ProtocolHistory coinsPerUtxoSize;


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof Protocols protocols)) {
      return false;
    }
    return Objects.equals(minFeeA, protocols.minFeeA) && Objects.equals(minFeeB,
                                                                        protocols.minFeeB)
        && Objects.equals(maxBlockSize, protocols.maxBlockSize) && Objects.equals(
        maxTxSize, protocols.maxTxSize) && Objects.equals(maxBhSize, protocols.maxBhSize)
        && Objects.equals(keyDeposit, protocols.keyDeposit) && Objects.equals(
        poolDeposit, protocols.poolDeposit) && Objects.equals(maxEpoch, protocols.maxEpoch)
        && Objects.equals(optimalPoolCount, protocols.optimalPoolCount)
        && Objects.equals(influence, protocols.influence) && Objects.equals(
        monetaryExpandRate, protocols.monetaryExpandRate) && Objects.equals(
        treasuryGrowthRate, protocols.treasuryGrowthRate) && Objects.equals(
        decentralisation, protocols.decentralisation) && Objects.equals(entropy,
                                                                        protocols.entropy)
        && Objects.equals(protocolMajor, protocols.protocolMajor)
        && Objects.equals(protocolMinor, protocols.protocolMinor)
        && Objects.equals(minUtxoValue, protocols.minUtxoValue) && Objects.equals(
        minPoolCost, protocols.minPoolCost) && Objects.equals(costModel,
                                                              protocols.costModel)
        && Objects.equals(priceMem, protocols.priceMem) && Objects.equals(
        priceStep, protocols.priceStep) && Objects.equals(maxTxExMem, protocols.maxTxExMem)
        && Objects.equals(maxTxExSteps, protocols.maxTxExSteps) && Objects.equals(
        maxBlockExMem, protocols.maxBlockExMem) && Objects.equals(maxBlockExSteps,
                                                                  protocols.maxBlockExSteps)
        && Objects.equals(maxValSize, protocols.maxValSize) && Objects.equals(
        collateralPercent, protocols.collateralPercent) && Objects.equals(
        maxCollateralInputs, protocols.maxCollateralInputs) && Objects.equals(
        coinsPerUtxoSize, protocols.coinsPerUtxoSize);
  }

  @Override
  public int hashCode() {
    return Objects.hash(minFeeA, minFeeB, maxBlockSize, maxTxSize, maxBhSize, keyDeposit,
                        poolDeposit,
                        maxEpoch, optimalPoolCount, influence, monetaryExpandRate,
                        treasuryGrowthRate,
                        decentralisation, entropy, protocolMajor, protocolMinor, minUtxoValue,
                        minPoolCost, costModel, priceMem, priceStep, maxTxExMem, maxTxExSteps,
                        maxBlockExMem, maxBlockExSteps, maxValSize, collateralPercent,
                        maxCollateralInputs, coinsPerUtxoSize);
  }
}
