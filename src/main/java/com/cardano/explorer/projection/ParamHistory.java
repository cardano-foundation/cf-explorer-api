package com.cardano.explorer.projection;

import com.bloxbean.cardano.client.transaction.spec.CostMdls;
import java.math.BigInteger;
import java.util.Date;

public interface ParamHistory {

  BigInteger getMinFeeA();

  BigInteger getMinFeeB();

  BigInteger getMaxBlockSize();

  BigInteger getMaxTxSize();

  BigInteger getMaxBhSize();

  BigInteger getKeyDeposit();

  BigInteger getPoolDeposit();

  BigInteger getMaxEpoch();

  BigInteger getOptimalPoolCount();

  BigInteger getMinUtxoValue();

  BigInteger getMinPoolCost();

  BigInteger getMaxTxExMem();

  BigInteger getMaxTxExSteps();

  BigInteger getMaxBlockExMem();

  BigInteger getMaxBlockExSteps();

  BigInteger getMaxValSize();

  BigInteger getCoinsPerUtxoSize();

  Double getInfluence();

  Double getMonetaryExpandRate();

  Double getTreasuryGrowthRate();

  Double getDecentralisation();

  Double getPriceMem();

  Double getPriceStep();

  Integer getProtocolMajor();

  Integer getProtocolMinor();

  Integer getCollateralPercent();

  Integer getMaxCollateralInputs();

  String getEntropy();

  CostMdls getCostModel();

  Date getTime();

  String getHash();
}
