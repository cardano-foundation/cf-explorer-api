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
