package com.cardano.explorer.model.response.protocol;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class Protocols {
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
}
