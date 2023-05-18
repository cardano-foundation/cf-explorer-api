package org.cardanofoundation.explorer.api.mapper;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.model.response.tx.ProtocolParamResponse;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface ProtocolMapper {

  String MAINNET = "mainnet";
  String PREPROD = "preprod";
  String MAINNET_NAME = "Mainnet";
  String TESTNET_NAME = "Testnet";

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

  default ProtocolParamResponse mapPreviousProtocolParamResponse(EpochParam epochParam,
                                                                 ProtocolParamResponse currentParam) {
    ProtocolParamResponse previousParam = new ProtocolParamResponse();

    if (Objects.nonNull(currentParam.getMinFeeA()) &&
        Objects.nonNull(epochParam.getMinFeeA()) &&
        Objects.isNull(previousParam.getMinFeeA())) {
      previousParam.setMinFeeA(epochParam.getMinFeeA());
    }

    if (Objects.nonNull(currentParam.getMinFeeB()) &&
        Objects.nonNull(epochParam.getMinFeeB()) &&
        Objects.isNull(previousParam.getMinFeeB())) {
      previousParam.setMinFeeB(epochParam.getMinFeeB());
    }

    if (Objects.nonNull(currentParam.getMaxBlockSize()) &&
        Objects.nonNull(epochParam.getMaxBlockSize()) &&
        Objects.isNull(previousParam.getMaxBlockSize())) {
      previousParam.setMaxBlockSize(epochParam.getMaxBlockSize());
    }

    if (Objects.nonNull(currentParam.getMaxTxSize()) &&
        Objects.nonNull(epochParam.getMaxBlockSize()) &&
        Objects.isNull(previousParam.getMaxTxSize())
    ) {
      previousParam.setMaxTxSize(epochParam.getMaxTxSize());
    }

    if (Objects.nonNull(currentParam.getMaxBhSize()) &&
        Objects.nonNull(epochParam.getMaxBhSize()) &&
        Objects.isNull(previousParam.getMaxBhSize())) {
      previousParam.setMaxBhSize(epochParam.getMaxBhSize());
    }

    if (Objects.nonNull(currentParam.getKeyDeposit()) &&
        Objects.nonNull(epochParam.getKeyDeposit()) &&
        Objects.isNull(previousParam.getKeyDeposit())) {
      previousParam.setKeyDeposit(epochParam.getKeyDeposit());
    }

    if (Objects.nonNull(currentParam.getPoolDeposit()) &&
        Objects.nonNull(epochParam.getPoolDeposit()) &&
        Objects.isNull(previousParam.getPoolDeposit())) {
      previousParam.setPoolDeposit(epochParam.getPoolDeposit());
    }

    if (Objects.nonNull(currentParam.getMaxEpoch()) &&
        Objects.nonNull(epochParam.getMaxEpoch()) &&
        Objects.isNull(previousParam.getMaxEpoch())) {
      previousParam.setMaxEpoch(epochParam.getMaxEpoch());
    }

    if (Objects.nonNull(currentParam.getOptimalPoolCount()) &&
        Objects.nonNull(epochParam.getOptimalPoolCount()) &&
        Objects.isNull(previousParam.getOptimalPoolCount())) {
      previousParam.setOptimalPoolCount(epochParam.getOptimalPoolCount());
    }

    if (Objects.nonNull(currentParam.getMinUtxoValue()) &&
        Objects.nonNull(epochParam.getMinUtxoValue()) &&
        Objects.isNull(previousParam.getMinUtxoValue())) {
      previousParam.setMinUtxoValue(epochParam.getMinUtxoValue());
    }

    if (Objects.nonNull(currentParam.getMinPoolCost()) &&
        Objects.nonNull(epochParam.getMinPoolCost()) &&
        Objects.isNull(previousParam.getMinPoolCost())) {
      previousParam.setMinPoolCost(epochParam.getMinPoolCost());
    }

    if (Objects.nonNull(currentParam.getMaxTxExMem()) &&
        Objects.nonNull(epochParam.getMaxTxExMem()) &&
        Objects.isNull(previousParam.getMaxTxExMem())) {
      previousParam.setMaxTxExMem(epochParam.getMaxTxExMem());
    }
    if (Objects.nonNull(currentParam.getMaxTxExSteps()) &&
        Objects.nonNull(epochParam.getMaxTxExSteps()) &&
        Objects.isNull(previousParam.getMaxTxExSteps())) {
      previousParam.setMaxTxExSteps(epochParam.getMaxTxExSteps());
    }

    if (Objects.nonNull(currentParam.getMaxBlockExMem()) &&
        Objects.nonNull(epochParam.getMaxBlockExMem()) &&
        Objects.isNull(previousParam.getMaxBlockExMem())) {
      previousParam.setMaxBlockExMem(epochParam.getMaxBlockExMem());
    }

    if (Objects.nonNull(currentParam.getMaxBlockExSteps()) &&
        Objects.nonNull(epochParam.getMaxBlockExSteps()) &&
        Objects.isNull(previousParam.getMaxBlockExSteps())) {
      previousParam.setMaxBlockExSteps(epochParam.getMaxBlockExSteps());
    }

    if (Objects.nonNull(currentParam.getMaxValSize()) &&
        Objects.nonNull(epochParam.getMaxValSize()) &&
        Objects.isNull(previousParam.getMaxValSize())) {
      previousParam.setMaxValSize(epochParam.getMaxValSize());
    }

    if (Objects.nonNull(currentParam.getCoinsPerUtxoSize()) &&
        Objects.nonNull(epochParam.getCoinsPerUtxoSize()) &&
        Objects.isNull(previousParam.getCoinsPerUtxoSize())) {
      previousParam.setCoinsPerUtxoSize(epochParam.getCoinsPerUtxoSize());
    }
    if (Objects.nonNull(currentParam.getInfluence()) &&
        Objects.nonNull(epochParam.getInfluence()) &&
        Objects.isNull(previousParam.getInfluence())) {
      previousParam.setInfluence(epochParam.getInfluence());
    }

    if (Objects.nonNull(currentParam.getMonetaryExpandRate()) &&
        Objects.nonNull(epochParam.getMonetaryExpandRate()) &&
        Objects.isNull(previousParam.getMonetaryExpandRate())) {
      previousParam.setMonetaryExpandRate(epochParam.getMonetaryExpandRate());
    }

    if (Objects.nonNull(currentParam.getTreasuryGrowthRate()) &&
        Objects.nonNull(epochParam.getTreasuryGrowthRate()) &&
        Objects.isNull(previousParam.getTreasuryGrowthRate())) {
      previousParam.setTreasuryGrowthRate(epochParam.getTreasuryGrowthRate());
    }

    if (Objects.nonNull(currentParam.getDecentralisation()) &&
        Objects.nonNull(epochParam.getDecentralisation()) &&
        Objects.isNull(previousParam.getDecentralisation())) {
      previousParam.setDecentralisation(epochParam.getDecentralisation());
    }

    if (Objects.nonNull(currentParam.getPriceMem()) &&
        Objects.nonNull(epochParam.getPriceMem()) &&
        Objects.isNull(previousParam.getPriceMem())) {
      previousParam.setPriceMem(epochParam.getPriceMem());
    }

    if (Objects.nonNull(currentParam.getPriceStep()) &&
        Objects.nonNull(epochParam.getPriceStep()) &&
        Objects.isNull(previousParam.getPriceStep())) {
      previousParam.setPriceStep(epochParam.getPriceStep());
    }

    if (Objects.nonNull(currentParam.getProtocolMajor()) &&
        Objects.nonNull(epochParam.getProtocolMajor()) &&
        Objects.isNull(previousParam.getProtocolMajor())) {
      previousParam.setProtocolMajor(epochParam.getProtocolMajor());
    }

    if (Objects.nonNull(currentParam.getProtocolMinor()) &&
        Objects.nonNull(epochParam.getProtocolMinor()) &&
        Objects.isNull(previousParam.getProtocolMinor())) {
      previousParam.setProtocolMinor(epochParam.getProtocolMinor());
    }

    if (Objects.nonNull(currentParam.getCollateralPercent()) &&
        Objects.nonNull(epochParam.getCollateralPercent()) &&
        Objects.isNull(previousParam.getCollateralPercent())) {
      currentParam.setCollateralPercent(epochParam.getCollateralPercent());
    }

    if (Objects.nonNull(currentParam.getMaxCollateralInputs()) &&
        Objects.nonNull(epochParam.getMaxCollateralInputs()) &&
        Objects.isNull(previousParam.getMaxCollateralInputs())) {
      previousParam.setMaxCollateralInputs(epochParam.getMaxCollateralInputs());
    }

    if (Objects.nonNull(currentParam.getEntropy()) &&
        Objects.nonNull(epochParam.getExtraEntropy()) &&
        Objects.isNull(previousParam.getEntropy())) {
      previousParam.setEntropy(epochParam.getExtraEntropy());
    }

    if (Objects.nonNull(epochParam.getCostModel()) &&
        Objects.nonNull(currentParam.getCostModel()) &&
        Objects.isNull(previousParam.getCostModel())) {
      previousParam.setCostModel(JsonUtil.getPrettyJson(epochParam.getCostModel().getCosts()));
    }

    return previousParam;
  }

  default HistoriesProtocol mapProtocolsToHistoriesProtocol(List<Protocols> protocols) {
    HistoriesProtocol historiesProtocol = HistoriesProtocol.builder()
        .minFeeA(new ArrayList<>())
        .minFeeB(new ArrayList<>())
        .maxBlockSize(new ArrayList<>())
        .maxTxSize(new ArrayList<>())
        .maxBhSize(new ArrayList<>())
        .keyDeposit(new ArrayList<>())
        .poolDeposit(new ArrayList<>())
        .maxEpoch(new ArrayList<>())
        .optimalPoolCount(new ArrayList<>())
        .influence(new ArrayList<>())
        .monetaryExpandRate(new ArrayList<>())
        .treasuryGrowthRate(new ArrayList<>())
        .decentralisation(new ArrayList<>())
        .entropy(new ArrayList<>())
        .protocolMajor(new ArrayList<>())
        .protocolMinor(new ArrayList<>())
        .minUtxoValue(new ArrayList<>())
        .minPoolCost(new ArrayList<>())
        .priceMem(new ArrayList<>())
        .priceStep(new ArrayList<>())
        .maxTxExMem(new ArrayList<>())
        .maxTxExSteps(new ArrayList<>())
        .maxBlockExMem(new ArrayList<>())
        .maxBlockExSteps(new ArrayList<>())
        .maxValSize(new ArrayList<>())
        .collateralPercent(new ArrayList<>())
        .maxCollateralInputs(new ArrayList<>())
        .coinsPerUtxoSize(new ArrayList<>())
        .epochChanges(new ArrayList<>())
        .costModel(new ArrayList<>())
        .build();

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


  default FixedProtocol mapFixedProtocol(String network) throws JsonProcessingException {
    switch (network) {
      case MAINNET:
        return FixedProtocol.builder()
            .activeSlotsCoeff(0.05F)
            .genDelegs(JsonUtil.parseJson("{\n"
                + "    \"ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c\": {\n"
                + "      \"delegate\": \"d9e5c76ad5ee778960804094a389f0b546b5c2b140a62f8ec43ea54d\",\n"
                + "      \"vrf\": \"64fa87e8b29a5b7bfbd6795677e3e878c505bc4a3649485d366b50abadec92d7\"\n"
                + "    },\n"
                + "    \"b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497\": {\n"
                + "      \"delegate\": \"855d6fc1e54274e331e34478eeac8d060b0b90c1f9e8a2b01167c048\",\n"
                + "      \"vrf\": \"66d5167a1f426bd1adcc8bbf4b88c280d38c148d135cb41e3f5a39f948ad7fcc\"\n"
                + "    },\n"
                + "    \"60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1\": {\n"
                + "      \"delegate\": \"7f72a1826ae3b279782ab2bc582d0d2958de65bd86b2c4f82d8ba956\",\n"
                + "      \"vrf\": \"c0546d9aa5740afd569d3c2d9c412595cd60822bb6d9a4e8ce6c43d12bd0f674\"\n"
                + "    },\n"
                + "    \"f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757\": {\n"
                + "      \"delegate\": \"69ae12f9e45c0c9122356c8e624b1fbbed6c22a2e3b4358cf0cb5011\",\n"
                + "      \"vrf\": \"6394a632af51a32768a6f12dac3485d9c0712d0b54e3f389f355385762a478f2\"\n"
                + "    },\n"
                + "    \"162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82\": {\n"
                + "      \"delegate\": \"4485708022839a7b9b8b639a939c85ec0ed6999b5b6dc651b03c43f6\",\n"
                + "      \"vrf\": \"aba81e764b71006c515986bf7b37a72fbb5554f78e6775f08e384dbd572a4b32\"\n"
                + "    },\n"
                + "    \"2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6\": {\n"
                + "      \"delegate\": \"6535db26347283990a252313a7903a45e3526ec25ddba381c071b25b\",\n"
                + "      \"vrf\": \"fcaca997b8105bd860876348fc2c6e68b13607f9bbd23515cd2193b555d267af\"\n"
                + "    },\n"
                + "    \"268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b\": {\n"
                + "      \"delegate\": \"1d4f2e1fda43070d71bb22a5522f86943c7c18aeb4fa47a362c27e23\",\n"
                + "      \"vrf\": \"63ef48bc5355f3e7973100c371d6a095251c80ceb40559f4750aa7014a6fb6db\"\n"
                + "    }\n"
                + "  }"))
            .updateQuorum(5)
            .networkId(MAINNET_NAME)
            .initialFunds("")
            .maxLovelaceSupply(BigInteger.valueOf(45000000000000000L))
            .networkMagic(764824073)
            .epochLength(432000)
            .systemStart("2017-09-23 21:44:51")
            .slotsPerKESPeriod(129600)
            .slotLength(1)
            .maxKESEvolutions(62)
            .securityParam(2160)
            .build();
      case PREPROD:
        return FixedProtocol.builder()
            .activeSlotsCoeff(0.05F)
            .genDelegs(JsonUtil.parseJson("{\n"
                + "        \"637f2e950b0fd8f8e3e811c5fbeb19e411e7a2bf37272b84b29c1a0b\": {\n"
                + "            \"delegate\": \"aae9293510344ddd636364c2673e34e03e79e3eefa8dbaa70e326f7d\",\n"
                + "            \"vrf\": \"227116365af2ed943f1a8b5e6557bfaa34996f1578eec667a5e2b361c51e4ce7\"\n"
                + "        },\n"
                + "        \"8a4b77c4f534f8b8cc6f269e5ebb7ba77fa63a476e50e05e66d7051c\": {\n"
                + "            \"delegate\": \"d15422b2e8b60e500a82a8f4ceaa98b04e55a0171d1125f6c58f8758\",\n"
                + "            \"vrf\": \"0ada6c25d62db5e1e35d3df727635afa943b9e8a123ab83785e2281605b09ce2\"\n"
                + "        },\n"
                + "        \"b00470cd193d67aac47c373602fccd4195aad3002c169b5570de1126\": {\n"
                + "            \"delegate\": \"b3b539e9e7ed1b32fbf778bf2ebf0a6b9f980eac90ac86623d11881a\",\n"
                + "            \"vrf\": \"0ff0ce9b820376e51c03b27877cd08f8ba40318f1a9f85a3db0b60dd03f71a7a\"\n"
                + "        },\n"
                + "        \"b260ffdb6eba541fcf18601923457307647dce807851b9d19da133ab\": {\n"
                + "            \"delegate\": \"7c64eb868b4ef566391a321c85323f41d2b95480d7ce56ad2abcb022\",\n"
                + "            \"vrf\": \"7fb22abd39d550c9a022ec8104648a26240a9ff9c88b8b89a6e20d393c03098e\"\n"
                + "        },\n"
                + "        \"ced1599fd821a39593e00592e5292bdc1437ae0f7af388ef5257344a\": {\n"
                + "            \"delegate\": \"de7ca985023cf892f4de7f5f1d0a7181668884752d9ebb9e96c95059\",\n"
                + "            \"vrf\": \"c301b7fc4d1b57fb60841bcec5e3d2db89602e5285801e522fce3790987b1124\"\n"
                + "        },\n"
                + "        \"dd2a7d71a05bed11db61555ba4c658cb1ce06c8024193d064f2a66ae\": {\n"
                + "            \"delegate\": \"1e113c218899ee7807f4028071d0e108fc790dade9fd1a0d0b0701ee\",\n"
                + "            \"vrf\": \"faf2702aa4893c877c622ab22dfeaf1d0c8aab98b837fe2bf667314f0d043822\"\n"
                + "        },\n"
                + "        \"f3b9e74f7d0f24d2314ea5dfbca94b65b2059d1ff94d97436b82d5b4\": {\n"
                + "            \"delegate\": \"fd637b08cc379ef7b99c83b416458fcda8a01a606041779331008fb9\",\n"
                + "            \"vrf\": \"37f2ea7c843a688159ddc2c38a2f997ab465150164a9136dca69564714b73268\"\n"
                + "        }\n"
                + "    }"))
            .updateQuorum(5)
            .networkId(TESTNET_NAME)
            .initialFunds("")
            .maxLovelaceSupply(BigInteger.valueOf(45000000000000000L))
            .networkMagic(1)
            .epochLength(432000)
            .systemStart("2022-06-01 00:00:00")
            .slotsPerKESPeriod(129600)
            .slotLength(1)
            .maxKESEvolutions(62)
            .securityParam(2160)
            .build();
      default:
        return null;
    }

  }
}
