package org.cardanofoundation.explorer.api.mapper;

import java.util.List;
import java.util.Objects;

import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.model.response.tx.ProtocolParamResponse;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
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
          || !protocolParam.getMonetaryExpandRate()
          .equals(paramProposal.getMonetaryExpandRate())) {
        protocolParam.setMonetaryExpandRate(paramProposal.getMonetaryExpandRate());
      }

      if (Objects.isNull(protocolParam.getTreasuryGrowthRate())
          || !protocolParam.getTreasuryGrowthRate()
          .equals(paramProposal.getTreasuryGrowthRate())) {
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

      var costModel = JsonUtil.getPrettyJson(paramProposal.getCostModel().getCosts());
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

  default ProtocolParamResponse mapPreviousProtocolParamResponse(List<ParamProposal> paramProposals,
      ProtocolParamResponse currentParam) {
    ProtocolParamResponse previousParam = new ProtocolParamResponse();

    paramProposals.forEach(paramProposal -> {
      if (Objects.nonNull(currentParam.getMinFeeA()) &&
          Objects.nonNull(paramProposal.getMinFeeA()) &&
          Objects.isNull(previousParam.getMinFeeA())) {
        previousParam.setMinFeeA(paramProposal.getMinFeeA());
      }

      if (Objects.nonNull(currentParam.getMinFeeB()) &&
          Objects.nonNull(paramProposal.getMinFeeB()) &&
          Objects.isNull(previousParam.getMinFeeB())) {
        previousParam.setMinFeeB(paramProposal.getMinFeeB());
      }

      if (Objects.nonNull(currentParam.getMaxBlockSize()) &&
          Objects.nonNull(paramProposal.getMaxBlockSize()) &&
          Objects.isNull(previousParam.getMaxBlockSize())) {
        previousParam.setMaxBlockSize(paramProposal.getMaxBlockSize());
      }

      if (Objects.nonNull(currentParam.getMaxTxSize()) &&
          Objects.nonNull(paramProposal.getMaxBlockSize()) &&
          Objects.isNull(previousParam.getMaxTxSize())
      ) {
        previousParam.setMaxTxSize(paramProposal.getMaxTxSize());
      }

      if (Objects.nonNull(currentParam.getMaxBhSize()) &&
          Objects.nonNull(paramProposal.getMaxBhSize()) &&
          Objects.isNull(previousParam.getMaxBhSize())) {
        previousParam.setMaxBhSize(paramProposal.getMaxBhSize());
      }

      if (Objects.nonNull(currentParam.getKeyDeposit()) &&
          Objects.nonNull(paramProposal.getKeyDeposit()) &&
          Objects.isNull(previousParam.getKeyDeposit())) {
        previousParam.setKeyDeposit(paramProposal.getKeyDeposit());
      }

      if (Objects.nonNull(currentParam.getPoolDeposit()) &&
          Objects.nonNull(paramProposal.getPoolDeposit()) &&
          Objects.isNull(previousParam.getPoolDeposit())) {
        previousParam.setPoolDeposit(paramProposal.getPoolDeposit());
      }

      if (Objects.nonNull(currentParam.getMaxEpoch()) &&
          Objects.nonNull(paramProposal.getMaxEpoch()) &&
          Objects.isNull(previousParam.getMaxEpoch())) {
        previousParam.setMaxEpoch(paramProposal.getMaxEpoch());
      }

      if (Objects.nonNull(currentParam.getOptimalPoolCount()) &&
          Objects.nonNull(paramProposal.getOptimalPoolCount()) &&
          Objects.isNull(previousParam.getOptimalPoolCount())) {
        previousParam.setOptimalPoolCount(paramProposal.getOptimalPoolCount());
      }

      if (Objects.nonNull(currentParam.getMinUtxoValue()) &&
          Objects.nonNull(paramProposal.getMinUtxoValue()) &&
          Objects.isNull(previousParam.getMinUtxoValue())) {
        previousParam.setMinUtxoValue(paramProposal.getMinUtxoValue());
      }

      if (Objects.nonNull(currentParam.getMinPoolCost()) &&
          Objects.nonNull(paramProposal.getMinPoolCost()) &&
          Objects.isNull(previousParam.getMinPoolCost())) {
        previousParam.setMinPoolCost(paramProposal.getMinPoolCost());
      }

      if (Objects.nonNull(currentParam.getMaxTxExMem()) &&
          Objects.nonNull(paramProposal.getMaxTxExMem()) &&
          Objects.isNull(previousParam.getMaxTxExMem())) {
        previousParam.setMaxTxExMem(paramProposal.getMaxTxExMem());
      }
      if (Objects.nonNull(currentParam.getMaxTxExSteps()) &&
          Objects.nonNull(paramProposal.getMaxTxExSteps()) &&
          Objects.isNull(previousParam.getMaxTxExSteps())) {
        previousParam.setMaxTxExSteps(paramProposal.getMaxTxExSteps());
      }

      if (Objects.nonNull(currentParam.getMaxBlockExMem()) &&
          Objects.nonNull(paramProposal.getMaxBlockExMem()) &&
          Objects.isNull(previousParam.getMaxBlockExMem())) {
        previousParam.setMaxBlockExMem(paramProposal.getMaxBlockExMem());
      }

      if (Objects.nonNull(currentParam.getMaxBlockExSteps()) &&
          Objects.nonNull(paramProposal.getMaxBlockExSteps()) &&
          Objects.isNull(previousParam.getMaxBlockExSteps())) {
        previousParam.setMaxBlockExSteps(paramProposal.getMaxBlockExSteps());
      }

      if (Objects.nonNull(currentParam.getMaxValSize()) &&
          Objects.nonNull(paramProposal.getMaxValSize()) &&
          Objects.isNull(previousParam.getMaxValSize())) {
        previousParam.setMaxValSize(paramProposal.getMaxValSize());
      }

      if (Objects.nonNull(currentParam.getCoinsPerUtxoSize()) &&
          Objects.nonNull(paramProposal.getCoinsPerUtxoSize()) &&
          Objects.isNull(previousParam.getCoinsPerUtxoSize())) {
        previousParam.setCoinsPerUtxoSize(paramProposal.getCoinsPerUtxoSize());
      }
      if (Objects.nonNull(currentParam.getInfluence()) &&
          Objects.nonNull(paramProposal.getInfluence()) &&
          Objects.isNull(previousParam.getInfluence())) {
        previousParam.setInfluence(paramProposal.getInfluence());
      }

      if (Objects.nonNull(currentParam.getMonetaryExpandRate()) &&
          Objects.nonNull(paramProposal.getMonetaryExpandRate()) &&
          Objects.isNull(previousParam.getMonetaryExpandRate())) {
        previousParam.setMonetaryExpandRate(paramProposal.getMonetaryExpandRate());
      }

      if (Objects.nonNull(currentParam.getTreasuryGrowthRate()) &&
          Objects.nonNull(paramProposal.getTreasuryGrowthRate()) &&
          Objects.isNull(previousParam.getTreasuryGrowthRate())) {
        previousParam.setTreasuryGrowthRate(paramProposal.getTreasuryGrowthRate());
      }

      if (Objects.nonNull(currentParam.getDecentralisation()) &&
          Objects.nonNull(paramProposal.getDecentralisation()) &&
          Objects.isNull(previousParam.getDecentralisation())) {
        previousParam.setDecentralisation(paramProposal.getDecentralisation());
      }

      if (Objects.nonNull(currentParam.getPriceMem()) &&
          Objects.nonNull(paramProposal.getPriceMem()) &&
          Objects.isNull(previousParam.getPriceMem())) {
        previousParam.setPriceMem(paramProposal.getPriceMem());
      }

      if (Objects.nonNull(currentParam.getPriceStep()) &&
          Objects.nonNull(paramProposal.getPriceStep()) &&
          Objects.isNull(previousParam.getPriceStep())) {
        previousParam.setPriceStep(paramProposal.getPriceStep());
      }

      if (Objects.nonNull(currentParam.getProtocolMajor()) &&
          Objects.nonNull(paramProposal.getProtocolMajor()) &&
          Objects.isNull(previousParam.getProtocolMajor())) {
        previousParam.setProtocolMajor(paramProposal.getProtocolMajor());
      }

      if (Objects.nonNull(currentParam.getProtocolMinor()) &&
          Objects.nonNull(paramProposal.getProtocolMinor()) &&
          Objects.isNull(previousParam.getProtocolMinor())) {
        previousParam.setProtocolMinor(paramProposal.getProtocolMinor());
      }

      if (Objects.nonNull(currentParam.getCollateralPercent()) &&
          Objects.nonNull(paramProposal.getCollateralPercent()) &&
          Objects.isNull(previousParam.getCollateralPercent())) {
        currentParam.setCollateralPercent(paramProposal.getCollateralPercent());
      }

      if (Objects.nonNull(currentParam.getMaxCollateralInputs()) &&
          Objects.nonNull(paramProposal.getMaxCollateralInputs()) &&
          Objects.isNull(previousParam.getMaxCollateralInputs())) {
        previousParam.setMaxCollateralInputs(paramProposal.getMaxCollateralInputs());
      }

      if (Objects.nonNull(currentParam.getEntropy()) &&
          Objects.nonNull(paramProposal.getEntropy()) &&
          Objects.isNull(previousParam.getEntropy())) {
        previousParam.setEntropy(paramProposal.getEntropy());
      }

      if (Objects.nonNull(paramProposal.getCostModel()) &&
          Objects.nonNull(currentParam.getCostModel()) &&
          Objects.isNull(previousParam.getCostModel())) {
        previousParam.setCostModel(JsonUtil.getPrettyJson(paramProposal.getCostModel().getCosts()));
      }
    });
    if (currentParam.hashCode() == new ProtocolParamResponse().hashCode()) {
      return null;
    }
    return previousParam;
  }

  default HistoriesProtocol mapProtocolsToHistoriesProtocol(List<Protocols> protocols){
    HistoriesProtocol historiesProtocol = new HistoriesProtocol();
    protocols.forEach(protocol -> {
      historiesProtocol.getEpochChanges().add(protocol.getEpochChange());
      historiesProtocol.getMinFeeA().add(protocol.getMinFeeA());
      historiesProtocol.getMinFeeB().add(protocol.getMinFeeB());
      historiesProtocol.getMaxBlockSize().add(protocol.getMaxBlockSize());
      historiesProtocol.getMaxTxSize().add(protocol.getMaxTxSize());
      historiesProtocol.getMaxBhSize().add(protocol.getMaxBhSize());
      historiesProtocol.getKeyDeposit().add(protocol.getKeyDeposit());
      historiesProtocol.getPoolDeposit().add(protocol.getPoolDeposit());
      historiesProtocol.getMaxEpoch().add(protocol.getMaxEpoch());
      historiesProtocol.getOptimalPoolCount().add(protocol.getOptimalPoolCount());
      historiesProtocol.getMinUtxoValue().add(protocol.getMinUtxoValue());
      historiesProtocol.getMinPoolCost().add(protocol.getMinPoolCost());
      historiesProtocol.getMaxTxExMem().add(protocol.getMaxTxExMem());
      historiesProtocol.getMaxTxExSteps().add(protocol.getMaxTxExSteps());
      historiesProtocol.getMaxBlockExMem().add(protocol.getMaxBlockExMem());
      historiesProtocol.getMaxBlockExSteps().add(protocol.getMaxBlockExSteps());
      historiesProtocol.getMaxValSize().add(protocol.getMaxValSize());
      historiesProtocol.getCoinsPerUtxoSize().add(protocol.getCoinsPerUtxoSize());
      historiesProtocol.getInfluence().add(protocol.getInfluence());
      historiesProtocol.getMonetaryExpandRate().add(protocol.getMonetaryExpandRate());
      historiesProtocol.getTreasuryGrowthRate().add(protocol.getTreasuryGrowthRate());
      historiesProtocol.getDecentralisation().add(protocol.getDecentralisation());
      historiesProtocol.getPriceMem().add(protocol.getPriceMem());
      historiesProtocol.getPriceStep().add(protocol.getPriceStep());
      historiesProtocol.getProtocolMajor().add(protocol.getProtocolMajor());
      historiesProtocol.getProtocolMinor().add(protocol.getProtocolMinor());
      historiesProtocol.getCollateralPercent().add(protocol.getCollateralPercent());
      historiesProtocol.getMaxCollateralInputs().add(protocol.getMaxCollateralInputs());
      historiesProtocol.getEntropy().add(protocol.getEntropy());
      historiesProtocol.getCostModel().add(protocol.getCostModel());
    });
    return historiesProtocol;
  }
}
