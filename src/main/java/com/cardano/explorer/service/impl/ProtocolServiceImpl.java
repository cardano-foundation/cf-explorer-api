package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import com.cardano.explorer.projection.ParamChange;
import com.cardano.explorer.projection.ParamHistory;
import com.cardano.explorer.repository.ParamProposalRepository;
import com.cardano.explorer.service.ProtocolParamService;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import org.springframework.stereotype.Service;

@Service
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolServiceImpl implements ProtocolParamService {

  final ParamProposalRepository paramProposalRepository;

  @Override
  public List<ProtocolHistory> getProtocolHistory(ProtocolType protocolType) {
    List<ParamHistory> paramHistories;
    Map<String, Object> protocols;

    List<ParamChange> historiesChange = paramProposalRepository.findHistoryTransactionForEachEpoch();

    List<Long> transactionIds = historiesChange.stream()
        .map(ParamChange::getTransaction)
        .collect(Collectors.toList());

    switch (protocolType) {
      case MIN_FEE_A:
        paramHistories = paramProposalRepository.getHistoryMinFeeA(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMinFeeA()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMinFeeA));
        break;
      case MIN_FEE_B:
        paramHistories = paramProposalRepository.getHistoryMinFeeB(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMinFeeB()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMinFeeB));
        break;
      case MAX_BLOCK_SIZE:
        paramHistories = paramProposalRepository.getHistoryMaxBhSize(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxTxSize()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxBlockSize));
        break;
      case MAX_TX_SIZE:
        paramHistories = paramProposalRepository.getHistoryMaxTxSize(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxTxSize()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxTxSize));
        break;
      case MAX_BH_SIZE:
        paramHistories = paramProposalRepository.getHistoryMaxBhSize(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxBhSize()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxBhSize));
        break;

      case KEY_DEPOSIT:
        paramHistories = paramProposalRepository.getHistoryKeyDeposit(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getKeyDeposit()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getKeyDeposit));
        break;

      case POOL_DEPOSIT:
        paramHistories = paramProposalRepository.getHistoryPoolDeposit(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getPoolDeposit()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getPoolDeposit));
        break;

      case MAX_EPOCH:
        paramHistories = paramProposalRepository.getHistoryMaxEpoch(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxEpoch()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxEpoch));
        break;

      case OPTIMAL_POOL_COUNT:
        paramHistories = paramProposalRepository.getHistoryOptimalPoolCount(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getOptimalPoolCount()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getOptimalPoolCount));
        break;

      case MIN_UTXO_VALUE:
        paramHistories = paramProposalRepository.getHistoryMinUtxoValue(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMinUtxoValue()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMinUtxoValue));
        break;

      case MIN_POOL_COST:
        paramHistories = paramProposalRepository.getHistoryMinPoolCost(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMinPoolCost()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMinPoolCost));
        break;

      case MAX_TX_EX_MEM:
        paramHistories = paramProposalRepository.getHistoryMaxTxExMem(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxTxExMem()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxTxExMem));
        break;

      case MAX_TX_EX_STEPS:
        paramHistories = paramProposalRepository.getHistoryMaxTxExSteps(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxTxExSteps()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxTxExSteps));
        break;

      case MAX_BLOCK_EX_MEM:
        paramHistories = paramProposalRepository.getHistoryMaxBlockExMem(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxTxExMem()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxBlockExMem));
        break;

      case MAX_BLOCK_EX_STEPS:
        paramHistories = paramProposalRepository.getHistoryMaxBlockExSteps(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxBlockExSteps()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxBlockExSteps));
        break;

      case MAX_VAL_SIZE:
        paramHistories = paramProposalRepository.getHistoryMaxValSize(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxValSize()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxValSize));
        break;

      case COINS_PER_UTXO_SIZE:
        paramHistories = paramProposalRepository.getHistoryCoinsPerUtxoSize(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getCoinsPerUtxoSize()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getCoinsPerUtxoSize));
        break;

      case INFLUENCE:
        paramHistories = paramProposalRepository.getHistoryInfluence(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getInfluence()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getInfluence));
        break;

      case MONETARY_EXPAND_RATE:
        paramHistories = paramProposalRepository.getHistoryMonetaryExpandRate(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMonetaryExpandRate()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMonetaryExpandRate));
        break;

      case TREASURY_GROWTH_RATE:
        paramHistories = paramProposalRepository.getHistoryTreasuryGrowthRate(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getTreasuryGrowthRate()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getTreasuryGrowthRate));
        break;

      case DECENTRALISATION:
        paramHistories = paramProposalRepository.getHistoryDecentralisation(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getDecentralisation()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getDecentralisation));
        break;

      case PRICE_MEM:
        paramHistories = paramProposalRepository.getHistoryPriceMem(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getPriceMem()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getPriceMem));
        break;

      case PRICE_STEP:
        paramHistories = paramProposalRepository.getHistoryPriceStep(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getPriceStep()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getPriceStep));
        break;

      case PROTOCOL_MAJOR:
        paramHistories = paramProposalRepository.getHistoryProtocolMajor(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getProtocolMajor()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getProtocolMajor));
        break;

      case PROTOCOL_MINOR:
        paramHistories = paramProposalRepository.getHistoryProtocolMinor(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getProtocolMinor()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getProtocolMinor));
        break;

      case COLLATERAL_PERCENT:
        paramHistories = paramProposalRepository.getHistoryCollateralPercent(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getCollateralPercent()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getCollateralPercent));
        break;

      case MAX_COLLATERAL_INPUTS:
        paramHistories = paramProposalRepository.getHistoryMaxCollateralInputs(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getMaxCollateralInputs()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getMaxCollateralInputs));
        break;

      case ENTROPY:
        paramHistories = paramProposalRepository.getHistoryEntropy(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getEntropy()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getEntropy));
        break;

      case COST_MODEL:
        paramHistories = paramProposalRepository.getHistoryCostModel(transactionIds);
        protocols = paramHistories.stream()
            .filter(paramHistory -> Objects.nonNull(paramHistory.getCostModel()))
            .collect(Collectors.toMap(ParamHistory::getHash, ParamHistory::getCostModel));
        break;
      default:
        throw new BusinessException(BusinessCode.PROTOCOL_NOT_FOUND);
    }

    return paramHistories.stream()
        .filter(paramHistory -> protocols.containsKey(paramHistory.getHash()))
        .map(paramHistory ->
            ProtocolHistory.builder()
                .value(protocols.get(paramHistory.getHash()))
                .time(paramHistory.getTime())
                .transactionHash(paramHistory.getHash())
                .build()
        )
        .sorted(Comparator.comparing(ProtocolHistory::getTime).reversed())
        .collect(Collectors.toList());
  }
}
