package com.cardano.explorer.model.response.tx;

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
  BigInteger minFeeA;


  @Override
  public int hashCode() {
    return hashCode(minFeeA, minFeeB, maxBlockSize, maxTxSize, maxBhSize, keyDeposit,
        poolDeposit,
        maxEpoch, optimalPoolCount, minUtxoValue, minPoolCost, maxTxExMem, maxTxExSteps,
        maxBlockExMem, maxBlockExSteps, maxValSize, coinsPerUtxoSize, influence, monetaryExpandRate,
        treasuryGrowthRate, decentralisation, priceMem, priceStep, protocolMajor, protocolMinor,
        collateralPercent, maxCollateralInputs, entropy, costModel);
  }

  public static int hashCode(Object... a) {
    if (a == null) {
      return -1;
    }

    int result = 1;

    for (Object element : a) {
      result = 31 * result + (element == null ? -1 : element.hashCode());
    }

    return result;
  }

  BigInteger minFeeB;

  BigInteger maxBlockSize;

  BigInteger maxTxSize;

  BigInteger maxBhSize;

  BigInteger keyDeposit;

  BigInteger poolDeposit;

  BigInteger maxEpoch;

  BigInteger optimalPoolCount;

  BigInteger minUtxoValue;

  BigInteger minPoolCost;

  BigInteger maxTxExMem;

  BigInteger maxTxExSteps;

  BigInteger maxBlockExMem;

  BigInteger maxBlockExSteps;

  BigInteger maxValSize;

  BigInteger coinsPerUtxoSize;

  Double influence;

  Double monetaryExpandRate;

  Double treasuryGrowthRate;

  Double decentralisation;

  Double priceMem;

  Double priceStep;

  Integer protocolMajor;

  Integer protocolMinor;

  Integer collateralPercent;

  Integer maxCollateralInputs;

  String entropy;

  String costModel;
}
