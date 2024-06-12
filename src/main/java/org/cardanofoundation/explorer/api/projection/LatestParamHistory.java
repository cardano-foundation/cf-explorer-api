package org.cardanofoundation.explorer.api.projection;

import java.util.Date;

public interface LatestParamHistory {
  Object getId();

  Object getMinFeeA();

  Object getMinFeeB();

  Object getMaxBlockSize();

  Object getMaxTxSize();

  Object getMaxBhSize();

  Object getKeyDeposit();

  Object getPoolDeposit();

  Object getMaxEpoch();

  Object getOptimalPoolCount();

  Object getMinUtxoValue();

  Object getMinPoolCost();

  Object getMaxTxExMem();

  Object getMaxTxExSteps();

  Object getMaxBlockExMem();

  Object getMaxBlockExSteps();

  Object getMaxValSize();

  Object getCoinsPerUtxoSize();

  Object getInfluence();

  Object getMonetaryExpandRate();

  Object getTreasuryGrowthRate();

  Object getDecentralisation();

  Object getPriceMem();

  Object getPriceStep();

  Object getProtocolMajor();

  Object getProtocolMinor();

  Object getCollateralPercent();

  Object getMaxCollateralInputs();

  String getEntropy();

  Object getCostModel();

  Date getBlockTime();

  Date getEpochTime();

  String getHash();

  Integer getEpochNo();

  // check protocol param is changed or not
  boolean getMinFeeAProposal();

  boolean getMinFeeBProposal();

  boolean getMaxBlockSizeProposal();

  boolean getMaxTxSizeProposal();

  boolean getMaxBhSizeProposal();

  boolean getKeyDepositProposal();

  boolean getPoolDepositProposal();

  boolean getMaxEpochProposal();

  boolean getOptimalPoolCountProposal();

  boolean getMinUtxoValueProposal();

  boolean getMinPoolCostProposal();

  boolean getMaxTxExMemProposal();

  boolean getMaxTxExStepsProposal();

  boolean getMaxBlockExMemProposal();

  boolean getMaxBlockExStepsProposal();

  boolean getMaxValSizeProposal();

  boolean getCoinsPerUtxoSizeProposal();

  boolean getInfluenceProposal();

  boolean getMonetaryExpandRateProposal();

  boolean getTreasuryGrowthRateProposal();

  boolean getDecentralisationProposal();

  boolean getPriceMemProposal();

  boolean getPriceStepProposal();

  boolean getProtocolMajorProposal();

  boolean getProtocolMinorProposal();

  boolean getCollateralPercentProposal();

  boolean getMaxCollateralInputsProposal();

  boolean getEntropyProposal();

  boolean getCostModelProposal();
}
