package com.cardano.explorer.mapper;

import com.cardano.explorer.model.response.tx.ProtocolParamResponse;
import com.sotatek.cardano.common.entity.ParamProposal;
import com.sotatek.cardano.ledgersync.util.JsonUtil;
import java.util.List;
import java.util.Objects;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface ProtocolMapper {

  default ProtocolParamResponse mapProtocolParamResponse(List<ParamProposal> paramProposals) {
    ProtocolParamResponse protocolParam = new ProtocolParamResponse();

    paramProposals.forEach(paramProposal -> {
      if (Objects.isNull(protocolParam.getMinFeeA())
          || !protocolParam.getMinFeeA().equals(paramProposal.getMinFeeA())) {
        protocolParam.setMinFeeA(paramProposal.getMinFeeA());
      }

      if (Objects.isNull(protocolParam.getMinFeeB())
          || !protocolParam.getMinFeeB().equals(paramProposal.getMinFeeB())) {
        protocolParam.setMinFeeB(paramProposal.getMinFeeB());
      }

      if (Objects.isNull(protocolParam.getMaxBlockSize())
          || !protocolParam.getMaxBlockSize().equals(paramProposal.getMaxBlockSize())) {
        protocolParam.setMaxBlockSize(paramProposal.getMaxBlockSize());
      }

      if (Objects.isNull(protocolParam.getMaxTxSize())
          || !protocolParam.getMaxTxSize().equals(paramProposal.getMaxTxSize())) {
        protocolParam.setMaxTxSize(paramProposal.getMaxTxSize());
      }

      if (Objects.isNull(protocolParam.getMaxBhSize())
          || !protocolParam.getMaxBhSize().equals(paramProposal.getMaxBhSize())) {
        protocolParam.setMaxBhSize(paramProposal.getMaxBhSize());
      }

      if (Objects.isNull(protocolParam.getKeyDeposit())
          || !protocolParam.getKeyDeposit().equals(paramProposal.getKeyDeposit())) {
        protocolParam.setKeyDeposit(paramProposal.getKeyDeposit());
      }

      if (Objects.isNull(protocolParam.getPoolDeposit())
          || !protocolParam.getPoolDeposit().equals(paramProposal.getPoolDeposit())) {
        protocolParam.setPoolDeposit(paramProposal.getPoolDeposit());
      }

      if (Objects.isNull(protocolParam.getMaxEpoch())
          || !protocolParam.getMaxEpoch().equals(paramProposal.getMaxEpoch())) {
        protocolParam.setMaxEpoch(paramProposal.getMaxEpoch());
      }

      if (Objects.isNull(protocolParam.getOptimalPoolCount())
          || !protocolParam.getOptimalPoolCount().equals(paramProposal.getOptimalPoolCount())) {
        protocolParam.setOptimalPoolCount(paramProposal.getOptimalPoolCount());
      }

      if (Objects.isNull(protocolParam.getMinUtxoValue())
          || !protocolParam.getMinUtxoValue().equals(paramProposal.getMinUtxoValue())) {
        protocolParam.setMinUtxoValue(paramProposal.getMinUtxoValue());
      }

      if (Objects.isNull(protocolParam.getMinPoolCost())
          || !protocolParam.getMinPoolCost().equals(paramProposal.getMinPoolCost())) {
        protocolParam.setMinPoolCost(paramProposal.getMinPoolCost());
      }

      if (Objects.isNull(protocolParam.getMaxTxExMem())
          || !protocolParam.getMaxTxExMem().equals(paramProposal.getMaxTxExMem())) {
        protocolParam.setMaxTxExMem(paramProposal.getMaxTxExMem());
      }
      if (Objects.isNull(protocolParam.getMaxTxExSteps())
          || !protocolParam.getMaxTxExSteps().equals(paramProposal.getMaxTxExSteps())) {
        protocolParam.setMaxTxExSteps(paramProposal.getMaxTxExSteps());
      }

      if (Objects.isNull(protocolParam.getMaxBlockExMem())
          || !protocolParam.getMaxBlockExMem().equals(paramProposal.getMaxBlockExMem())) {
        protocolParam.setMaxBlockExMem(paramProposal.getMaxBlockExMem());
      }

      if (Objects.isNull(protocolParam.getMaxBlockExSteps())
          || !protocolParam.getMaxBlockExSteps().equals(paramProposal.getMaxBlockExSteps())) {
        protocolParam.setMaxBlockExSteps(paramProposal.getMaxBlockExSteps());
      }

      if (Objects.isNull(protocolParam.getMaxValSize())
          || !protocolParam.getMaxValSize().equals(paramProposal.getMaxValSize())) {
        protocolParam.setMaxValSize(paramProposal.getMaxValSize());
      }

      if (Objects.isNull(protocolParam.getCoinsPerUtxoSize())
          || !protocolParam.getCoinsPerUtxoSize().equals(paramProposal.getCoinsPerUtxoSize())) {
        protocolParam.setCoinsPerUtxoSize(paramProposal.getCoinsPerUtxoSize());
      }
      if (Objects.isNull(protocolParam.getInfluence())
          || !protocolParam.getInfluence().equals(paramProposal.getInfluence())) {
        protocolParam.setInfluence(paramProposal.getInfluence());
      }

      if (Objects.isNull(protocolParam.getMonetaryExpandRate())
          || !protocolParam.getMonetaryExpandRate().equals(paramProposal.getMonetaryExpandRate())) {
        protocolParam.setMonetaryExpandRate(paramProposal.getMonetaryExpandRate());
      }

      if (Objects.isNull(protocolParam.getTreasuryGrowthRate())
          || !protocolParam.getTreasuryGrowthRate().equals(paramProposal.getTreasuryGrowthRate())) {
        protocolParam.setTreasuryGrowthRate(paramProposal.getTreasuryGrowthRate());
      }

      if (Objects.isNull(protocolParam.getDecentralisation())
          || !protocolParam.getDecentralisation().equals(paramProposal.getDecentralisation())) {
        protocolParam.setDecentralisation(paramProposal.getDecentralisation());
      }

      if (Objects.isNull(protocolParam.getPriceMem())
          || !protocolParam.getPriceMem().equals(paramProposal.getPriceMem())) {
        protocolParam.setPriceMem(paramProposal.getPriceMem());
      }

      if (Objects.isNull(protocolParam.getPriceStep())
          || !protocolParam.getPriceStep().equals(paramProposal.getPriceStep())) {
        protocolParam.setPriceStep(paramProposal.getPriceStep());
      }

      if (Objects.isNull(protocolParam.getProtocolMajor())
          || !protocolParam.getProtocolMajor().equals(paramProposal.getProtocolMajor())) {
        protocolParam.setProtocolMajor(paramProposal.getProtocolMajor());
      }

      if (Objects.isNull(protocolParam.getProtocolMinor())
          || !protocolParam.getProtocolMinor().equals(paramProposal.getProtocolMinor())) {
        protocolParam.setProtocolMinor(paramProposal.getProtocolMinor());
      }

      if (Objects.isNull(protocolParam.getCollateralPercent())
          || !protocolParam.getCollateralPercent().equals(paramProposal.getCollateralPercent())) {
        protocolParam.setCollateralPercent(paramProposal.getCollateralPercent());
      }

      if (Objects.isNull(protocolParam.getMaxCollateralInputs())
          || !protocolParam.getMaxCollateralInputs()
          .equals(paramProposal.getMaxCollateralInputs())) {
        protocolParam.setMaxCollateralInputs(paramProposal.getMaxCollateralInputs());
      }

      if (Objects.isNull(protocolParam.getEntropy())
          || !protocolParam.getEntropy().equals(paramProposal.getEntropy())) {
        protocolParam.setEntropy(paramProposal.getEntropy());
      }

      if (Objects.isNull(paramProposal.getCostModel())) {
        return;
      }

      var costModel = JsonUtil.getPrettyJson(paramProposal.getCostModel());
      if (Objects.isNull(protocolParam.getCostModel())
          || !protocolParam.getCostModel().equals(costModel)) {
        protocolParam.setCostModel(costModel);
      }
    });
    if (protocolParam.hashCode() == new ProtocolParamResponse().hashCode()) {
      return null;
    }
    return protocolParam;
  }

}
