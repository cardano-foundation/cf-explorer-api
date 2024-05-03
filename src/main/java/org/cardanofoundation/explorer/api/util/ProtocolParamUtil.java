package org.cardanofoundation.explorer.api.util;

import java.util.ArrayList;
import java.util.List;

import org.cardanofoundation.explorer.api.common.enumeration.ProtocolParamGroup;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;

public class ProtocolParamUtil {
  private ProtocolParamUtil() {}

  public static List<ProtocolParamGroup> getGroupsWithNonNullField(EpochParam params) {
    List<ProtocolParamGroup> groups = new ArrayList<>();
    // todo: Economic: check monetary expansion (rho), treasury expansion (tau), minimum Lovelace
    // deposit per byte of serialized UTxO (coinsPerUTxOByte)
    if (isNonNull(
        params.getMinFeeA(),
        params.getMinFeeB(),
        params.getKeyDeposit(),
        params.getPoolDeposit(),
        params.getMinPoolCost(),
        params.getPriceMem(),
        params.getPriceStep())) {
      groups.add(ProtocolParamGroup.ECONOMIC);
    }

    if (isNonNull(
        params.getMaxBlockSize(),
        params.getMaxTxSize(),
        params.getMaxBhSize(),
        params.getMaxValSize(),
        params.getMaxTxExMem(),
        params.getMaxTxExSteps(),
        params.getMaxBlockExMem(),
        params.getMaxBlockExSteps(),
        params.getMaxCollateralInputs())) {
      groups.add(ProtocolParamGroup.NETWORK);
    }
    // todo: Technical: check pool retirement maximum epoch (eMax),
    /* pool pledge influence (a0) as OptimalPoolCount, desired number of pools (nOpt) as Influence
     *  Plutus execution cost models (costModels) as CostModel, proportion of collateral needed for scripts (collateralPercentage) as CollateralPercent */
    if (isNonNull(
        params.getOptimalPoolCount(),
        params.getInfluence(),
        params.getCostModel(),
        params.getCollateralPercent())) {
      groups.add(ProtocolParamGroup.TECHNICAL);
    }

    if (isNonNull(
        params.getPvtMotionNoConfidence(),
        params.getPvtCommitteeNormal(),
        params.getPvtCommitteeNoConfidence(),
        params.getPvtHardForkInitiation(),
        params.getPvtPPSecurityGroup(),
        params.getDvtMotionNoConfidence(),
        params.getDvtCommitteeNormal(),
        params.getDvtCommitteeNoConfidence(),
        params.getDvtUpdateToConstitution(),
        params.getDvtHardForkInitiation(),
        params.getDvtPPNetworkGroup(),
        params.getDvtPPEconomicGroup(),
        params.getDvtPPTechnicalGroup(),
        params.getDvtPPGovGroup(),
        params.getDvtTreasuryWithdrawal(),
        params.getCommitteeMinSize(),
        params.getCommitteeMaxTermLength(),
        params.getGovActionLifetime(),
        params.getGovActionDeposit(),
        params.getDrepDeposit(),
        params.getDrepActivity())) {
      groups.add(ProtocolParamGroup.GOVERNANCE);
    }
    /// todo: Security: check minimum Lovelace deposit per byte of serialized UTxO
    // (coinsPerUTxOByte)
    if (isNonNull(
        params.getMaxBlockSize(),
        params.getMaxTxSize(),
        params.getMaxBhSize(),
        params.getMaxValSize(),
        params.getMaxBlockExMem(),
        params.getMaxBlockExSteps(),
        params.getMinFeeA(),
        params.getMinFeeB(),
        params.getGovActionDeposit(),
        params.getMinFeeRefScriptCostPerByte())) {
      groups.add(ProtocolParamGroup.SECURITY);
    }

    return groups;
  }

  private static boolean isNonNull(Object... objects) {
    for (Object obj : objects) {
      if (obj != null) {
        return true;
      }
    }
    return false;
  }
}
