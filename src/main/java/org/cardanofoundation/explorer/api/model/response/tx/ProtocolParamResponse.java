package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProtocolParamResponse {
  Object minFeeA;

  Object minFeeB;

  Object maxBlockSize;

  Object maxTxSize;

  Object maxBhSize;

  Object keyDeposit;

  Object poolDeposit;

  Object maxEpoch;

  Object optimalPoolCount;

  Object minUtxoValue;

  Object minPoolCost;

  Object maxTxExMem;

  Object maxTxExSteps;

  Object maxBlockExMem;

  Object maxBlockExSteps;

  Object maxValSize;

  Object coinsPerUtxoSize;

  Object influence;

  Object monetaryExpandRate;

  Object treasuryGrowthRate;

  Object decentralisation;

  Object priceMem;

  Object priceStep;

  Object protocolMajor;

  Object protocolMinor;

  Object collateralPercent;

  Object maxCollateralInputs;

  Object entropy;

  Object costModel;


  @Override
  public int hashCode() {
    return CommonConstant.hashCode(minFeeA, minFeeB, maxBlockSize, maxTxSize, maxBhSize, keyDeposit,
        poolDeposit,
        maxEpoch, optimalPoolCount, minUtxoValue, minPoolCost, maxTxExMem, maxTxExSteps,
        maxBlockExMem, maxBlockExSteps, maxValSize, coinsPerUtxoSize, influence, monetaryExpandRate,
        treasuryGrowthRate, decentralisation, priceMem, priceStep, protocolMajor, protocolMinor,
        collateralPercent, maxCollateralInputs, entropy, costModel);
  }

  @JsonIgnore
  public boolean isNull() {
    if(this.hashCode() == new ProtocolParamResponse().hashCode()) {
      return true;
    }
    return false;
  }
}
