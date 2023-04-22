package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import com.cardano.explorer.model.response.protocol.Protocols;
import com.cardano.explorer.projection.ParamHistory;
import com.cardano.explorer.repository.EpochParamRepository;
import com.cardano.explorer.repository.ParamProposalRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.ProtocolParamService;
import com.sotatek.cardano.common.entity.EpochParam;
import com.sotatek.cardano.common.entity.ParamProposal;
import com.sotatek.cardano.common.entity.Tx;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.PostConstruct;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolParamServiceImpl implements ProtocolParamService {

  public static final String GET = "get";
  final ParamProposalRepository paramProposalRepository;
  final EpochParamRepository epochParamRepository;
  final TxRepository txRepository;

  Map<String, Method> paramProtocolMethod;

  @Override
  public Set<ProtocolHistory> getProtocolHistory(ProtocolType protocolType) {
    Stream<ParamProposal> historiesChange =
        paramProposalRepository.getAllDistinctProtocolParam(BigInteger.ZERO.longValue())
            .stream();
    if (protocolType.equals(ProtocolType.COST_MODEL)) {
      return historiesChange
          .filter(paramProposal -> Objects.nonNull(paramProposal.getCostModel()))
          .map(paramProposal ->
              ProtocolHistory
                  .builder()
                  .value(paramProposal.getCostModel().getCosts())
                  .transactionHash(paramProposal.getRegisteredTx().getHash())
                  .time(paramProposal.getRegisteredTx().getBlock().getTime())
                  .build())
          .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    Stream<ProtocolHistory> historyStream;
    historyStream = getHistoryStream(historiesChange, paramProposal -> {
      try {
        return paramProtocolMethod.get(protocolType.getFieldName()).invoke(paramProposal);
      } catch (IllegalAccessException | InvocationTargetException e) {
        log.error(e.getMessage());
        return null; //  we filter is anyway
      }
    });

    return historyStream.collect(Collectors.toCollection(LinkedHashSet::new));
  }

  @Override
  public Protocols getProtocolCurrentHistory() {
    EpochParam currentProtocols = epochParamRepository.findTopEpochParam();

    List<ParamHistory> previousProtocolsChange = paramProposalRepository
        .findProtocolsChange(currentProtocols.getEpochNo())
        .stream()
        .sorted(Comparator.comparing(ParamHistory::getId))
        .collect(Collectors.toList());

    Map<Long, Tx> txs = txRepository.findByIdIn(
            previousProtocolsChange.stream().map(ParamHistory::getTx).collect(
                Collectors.toList()))
        .stream()
        .collect(Collectors.toMap(Tx::getId, Function.identity()));

    return Protocols.builder()
        .minFeeA(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMinFeeA,
            currentProtocols.getMinFeeA(), txs))
        .minFeeB(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMinFeeB,
            currentProtocols.getMinFeeB(), txs))
        .maxBlockSize(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxBlockSize,
            currentProtocols.getMaxBlockSize(), txs))
        .maxTxSize(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxTxSize,
            currentProtocols.getMaxTxSize(), txs))
        .maxBhSize(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxBhSize,
            currentProtocols.getMaxBhSize(), txs))
        .keyDeposit(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getKeyDeposit,
            currentProtocols.getKeyDeposit(), txs))
        .poolDeposit(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getPoolDeposit,
            currentProtocols.getPoolDeposit(), txs))
        .maxEpoch(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxEpoch,
            currentProtocols.getMaxEpoch(), txs))
        .optimalPoolCount(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getOptimalPoolCount,
            currentProtocols.getOptimalPoolCount(), txs))
        .influence(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getInfluence,
            currentProtocols.getInfluence(), txs))
        .monetaryExpandRate(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMonetaryExpandRate,
            currentProtocols.getMonetaryExpandRate(), txs))
        .treasuryGrowthRate(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getTreasuryGrowthRate,
            currentProtocols.getTreasuryGrowthRate(), txs))
        .decentralisation(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getDecentralisation,
            currentProtocols.getDecentralisation(), txs))
        .entropy(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getEntropy,
            currentProtocols.getExtraEntropy(), txs))
        .protocolMajor(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getProtocolMajor,
            currentProtocols.getProtocolMajor(), txs))
        .protocolMinor(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getProtocolMinor,
            currentProtocols.getProtocolMinor(), txs))
        .minUtxoValue(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMinUtxoValue,
            currentProtocols.getMinUtxoValue(), txs))
        .minPoolCost(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMinPoolCost,
            currentProtocols.getMinPoolCost(), txs))
        .priceMem(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getPriceMem,
            currentProtocols.getPriceMem(), txs))
        .priceStep(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getPriceStep,
            currentProtocols.getPriceStep(), txs))
        .maxTxExMem(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxTxExMem,
            currentProtocols.getMaxTxExMem(), txs))
        .maxTxExSteps(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxTxExSteps,
            currentProtocols.getMaxTxExSteps(), txs))
        .maxBlockExMem(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxBlockExMem,
            currentProtocols.getMaxBlockExMem(), txs))
        .maxBlockExSteps(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxBlockExSteps,
            currentProtocols.getMaxBlockExSteps(), txs))
        .maxValSize(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxValSize,
            currentProtocols.getMaxValSize(), txs))
        .collateralPercent(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getCollateralPercent,
            currentProtocols.getCollateralPercent(), txs))
        .maxCollateralInputs(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getMaxCollateralInputs,
            currentProtocols.getMaxCollateralInputs(), txs))
        .coinsPerUtxoSize(getChangeProtocol(previousProtocolsChange.stream(),
            ParamHistory::getCoinsPerUtxoSize,
            currentProtocols.getCoinsPerUtxoSize(), txs))
        .build();
  }

  private Stream<ProtocolHistory> getHistoryStream(Stream<ParamProposal> historiesChange,
      Function<ParamProposal, ?> function) {

    return historiesChange
        .filter(paramProposal -> Objects.nonNull(function.apply(paramProposal)))
        .map(paramProposal ->
            ProtocolHistory
                .builder()
                .value(function.apply(paramProposal))
                .transactionHash(paramProposal.getRegisteredTx().getHash())
                .time(paramProposal.getRegisteredTx().getBlock().getTime())
                .build()
        );
  }

  private ProtocolHistory getChangeProtocol(Stream<ParamHistory> proposalChange,
      Function<ParamHistory, ?> function,
      Object currentProtocol,
      Map<Long, Tx> txs) {
    var history = proposalChange
        .filter(paramProposal -> Objects.nonNull(function.apply(paramProposal)))
        .findFirst()
        .orElse(null);

    if (Objects.isNull(history)) {
      return ProtocolHistory.builder()
          .value(currentProtocol)
          .time(null)
          .transactionHash(null)
          .build();
    }

    Tx tx = txs.get(history.getTx());

    return ProtocolHistory.builder()
        .value(currentProtocol)
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build();
  }


  public
  @PostConstruct
  void setup() {
    paramProtocolMethod = new HashMap<>();
    Field[] fields = ParamProposal.class.getDeclaredFields();
    Method[] methods = ParamProposal.class.getDeclaredMethods();

    for (Field field : fields) {
      Method methodUsed = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.getName().toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          })
          .findFirst()
          .orElse(null); // Method null is ok, because we not use it anyway
      if (Objects.nonNull(methodUsed)) {
        paramProtocolMethod.put(field.getName(), methodUsed);
      }
    }
  }
}
