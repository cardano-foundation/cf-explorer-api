package org.cardanofoundation.explorer.api.model.response.tx;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProtocolParamResponse {

  public static final int HASH_LENGTH = 31;
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

  public int hashCode(Object... a) {
    if (a == null) {
      return -BigInteger.ONE.intValue();
    }

    int result = BigInteger.ONE.intValue();

    for (Object element : a) {
      result = HASH_LENGTH * result + (element == null ? -BigInteger.ONE.intValue() : element.hashCode());
    }

    return result;
  }
  @Override
  public int hashCode() {
    return hashCode(minFeeA, minFeeB, maxBlockSize, maxTxSize, maxBhSize, keyDeposit,
        poolDeposit,
        maxEpoch, optimalPoolCount, minUtxoValue, minPoolCost, maxTxExMem, maxTxExSteps,
        maxBlockExMem, maxBlockExSteps, maxValSize, coinsPerUtxoSize, influence, monetaryExpandRate,
        treasuryGrowthRate, decentralisation, priceMem, priceStep, protocolMajor, protocolMinor,
        collateralPercent, maxCollateralInputs, entropy, costModel);
  }

  public boolean isNull() {
    if(this.hashCode() == new ProtocolParamResponse().hashCode()) {
      return true;
    }
    return false;
  }
}
