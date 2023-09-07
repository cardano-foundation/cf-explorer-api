package org.cardanofoundation.explorer.api.mapper;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.model.response.tx.ProtocolParamResponse;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import org.mapstruct.Mapper;
import org.springframework.data.util.Pair;
import org.springframework.util.ObjectUtils;

@Mapper(componentModel = "spring")
public interface ProtocolMapper {

  String MAINNET = "mainnet";
  String PREPROD = "preprod";
  String PREVIEW = "preview";
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

  default HistoriesProtocol mapProtocolsToHistoriesProtocol(List<Protocols> protocols,
                                                            Map<ProtocolType, Pair<Method, Method>> protocolsMethods,
                                                            Map<ProtocolType, Pair<Method, Method>> historiesProtocolMethods,
                                                            List<ProtocolType> protocolTypes) {
    HistoriesProtocol historiesProtocol = HistoriesProtocol.builder()
        .epochChanges(new ArrayList<>())
        .build();

    protocols.forEach(protocol -> {
      historiesProtocol.getEpochChanges().add(protocol.getEpochChange());
      protocolTypes.forEach(protocolType -> {
        var historiesGetter = historiesProtocolMethods.get(protocolType).getSecond();
        var historiesSetter = historiesProtocolMethods.get(protocolType).getFirst();

        var protocolGetter = protocolsMethods.get(protocolType).getSecond();

        try {
          var protocolValue = protocolGetter.invoke(protocol);

          if (Objects.nonNull(protocolValue)) {
            if (ObjectUtils.isEmpty(historiesGetter.invoke(historiesProtocol))) {
              historiesSetter.invoke(historiesProtocol, new ArrayList<>());
            }
            ((List<ProtocolHistory>) historiesGetter.invoke(historiesProtocol)).add(
                (ProtocolHistory) protocolValue);
          }
        } catch (Exception e) {
          e.printStackTrace();
        }
      });
    });

    return historiesProtocol;
  }
}
