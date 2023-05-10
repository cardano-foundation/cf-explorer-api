package org.cardanofoundation.explorer.api.service.impl;

import jakarta.annotation.PostConstruct;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.api.repository.CostModelRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.consumercommon.entity.CostModel;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolParamServiceImpl implements ProtocolParamService {

  final ParamProposalRepository paramProposalRepository;
  final EpochParamRepository epochParamRepository;
  final TxRepository txRepository;
  final CostModelRepository costModelRepository;
  public static final String GET = "get";
  Map<ProtocolType, Method> paramProtocolMethod;

  @Override
  public List<Protocols> getHistoryProtocolParam() {
    List<ParamHistory> historiesChange = paramProposalRepository.findProtocolsChange();
    Map<Integer, EpochParam> epochParams = epochParamRepository.findAll().stream()
        .collect(Collectors.toMap(EpochParam::getEpochNo, Function.identity(), (a, b) -> a));

    Map<Integer, List<ParamHistory>> historiesChangeByEpoch = historiesChange
        .parallelStream()
        .collect(Collectors.groupingBy(ParamHistory::getEpochNo, Collectors.toList()));

    Map<Long, Tx> txs = txRepository.findByIdIn(
            historiesChange.stream().map(ParamHistory::getTx).toList())
        .parallelStream().collect(Collectors.toMap(Tx::getId, Function.identity()));

    Map<Integer, Protocols> unprocessProtocols = historiesChangeByEpoch.entrySet().stream()
        .sorted((paramOld, paramNew) -> paramNew.getKey().compareTo(paramOld.getKey()))
        .map(entry -> {
          Protocols protocols = getEpochProtocol(entry.getKey());
          Protocols protocolsChange = getProtocolChangeInOneEpoch(entry.getValue(), txs);

          if (Objects.equals(protocols, protocolsChange)) {
            return protocols;
          }
          return protocolsChange;
        }).collect(Collectors.toMap(Protocols::getStartEpoch, Function.identity()));

    List<Protocols> processProtocols = new ArrayList<>();

    final Set<Integer> epochs = historiesChangeByEpoch.keySet();

    if (ObjectUtils.isEmpty(epochs)) {
      return Collections.emptyList();
    }

    final Integer min = epochs.stream().min(Integer::compareTo).orElse(BigInteger.ZERO.intValue());
    final Integer max = epochs.stream().max(Integer::compareTo).orElse(BigInteger.ZERO.intValue())
        + BigInteger.ONE.intValue();

    AtomicReference<Protocols> protocols = new AtomicReference<>(
        unprocessProtocols.get(max - BigInteger.ONE.intValue()));

    IntStream.range(min, max)
        .boxed()
        .sorted(Collections.reverseOrder())
        .forEach(epoch -> {

          Protocols markProtocol = protocols.get();
          Protocols currentProtocol = unprocessProtocols.get(epoch);

          if (Objects.isNull(currentProtocol)) {
            currentProtocol = mapProtocols(epochParams.get(epoch));
          }

          if (Objects.equals(markProtocol, currentProtocol)) {
            markProtocol.setEndEpoch(epoch);
            return;
          }

          fillMissingProtocolField(markProtocol, epochParams.get(epoch));

          processProtocols.add(protocols.get());
          protocols.set(currentProtocol);
        });

    return processProtocols;
  }

  @Override
  public Protocols getLatestChange() {
    Integer epoch = paramProposalRepository.findMaxEpoch();
    List<ParamHistory> paramHistories = paramProposalRepository.findEpochProtocolsChange(epoch);
    Optional<EpochParam> epochParamOptional = epochParamRepository.findEpochParamByEpochNo(epoch);

    return epochParamOptional
        .map(epochParam -> {

          Map<Long, Tx> txs = txRepository.findByIdIn(
                  paramHistories.stream().map(ParamHistory::getTx).toList())
              .parallelStream().collect(Collectors.toMap(Tx::getId, Function.identity()));

          Protocols epochChange = getProtocolChangeInOneEpoch(paramHistories, txs);
          epochChange.setStartEpoch(epoch);
          fillMissingProtocolField(epochChange, epochParam);
          return epochChange;
        }).orElse(Protocols.builder().build());
  }

  @Override
  public Protocols getFixedProtocols() {
    Protocols fixedProtocol = Protocols.builder().build();
    List<EpochParam> epochParams = epochParamRepository.findAll()
        .stream()
        .sorted(Comparator.comparing(EpochParam::getId).reversed())
        .toList();

    if (ObjectUtils.isEmpty(epochParams)) {
      return fixedProtocol;
    }

    EpochParam maxEpochParam = epochParams.stream()
        .max(Comparator.comparing(EpochParam::getEpochNo)).get();

    EpochParam minEpochParam = epochParams.stream()
        .min(Comparator.comparing(EpochParam::getEpochNo)).get();

    fixedProtocol.setStartEpoch(maxEpochParam.getEpochNo());
    fixedProtocol.setEndEpoch(minEpochParam.getEpochNo());

    Map<ProtocolType, Object> fixedProtocolMap = new ConcurrentHashMap<>();

    paramProtocolMethod.keySet()
        .forEach(key -> {
          try {
            Object object = paramProtocolMethod.get(key).invoke(maxEpochParam);
            if (Objects.nonNull(object)) {
              fixedProtocolMap.put(key, object);
            }
          } catch (IllegalAccessException | InvocationTargetException e) {
            log.error(e.getMessage());
          }
        });

    epochParams.forEach(
        epochParam ->
            fixedProtocolMap.keySet()
                .forEach(key -> {
                  try {
                    Object currentValue = fixedProtocolMap.get(key);
                    Object oldValue = paramProtocolMethod.get(key)
                        .invoke(epochParam);

                    if (!Objects.equals(currentValue, oldValue) && Objects.nonNull(oldValue)) {
                      fixedProtocolMap.remove(key);
                    }
                  } catch (IllegalAccessException |
                           InvocationTargetException e) {
                    log.error(e.getMessage());
                  }
                })
    );
    fillFixedProtocolField(fixedProtocolMap, fixedProtocol);
    return fixedProtocol;
  }

  private Protocols getEpochProtocol(Integer epochNo) {
    return Protocols.builder()
        .startEpoch(epochNo)
        .endEpoch(epochNo)
        .build();
  }

  private Protocols getProtocolChangeInOneEpoch(List<ParamHistory> paramHistories,
                                                Map<Long, Tx> txs) {
    Protocols protocols = getEpochProtocol(
        paramHistories.get(BigInteger.ZERO.intValue()).getEpochNo());
    mapProtocols(paramHistories, protocols, txs);
    return protocols;
  }

  private ProtocolHistory getChangeProtocol(Object currentProtocol, Tx tx) {

    return ProtocolHistory.builder()
        .value(currentProtocol)
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build();
  }

  private ProtocolHistory getChangeCostModelProtocol(Long costModelId,
                                                     Tx tx) {
    Optional<CostModel> costModel = costModelRepository.findById(costModelId);

    return costModel.map(model -> ProtocolHistory.builder()
        .value(model.getCosts())
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build()).orElse(null);
  }

  private ProtocolHistory getChangeCostModelProtocol(String cost) {
    return ProtocolHistory.builder()
        .value(cost)
        .time(null)
        .transactionHash(null)
        .build();
  }

  private ProtocolHistory getChangeProtocol(Object object) {
    return ProtocolHistory.builder()
        .value(object)
        .time(null)
        .transactionHash(null)
        .build();
  }

  private void mapProtocols(List<ParamHistory> paramProposals, Protocols protocols,
                            Map<Long, Tx> txs) {
    paramProposals
        .stream()
        .takeWhile(paramProposal -> Objects.isNull(protocols.getMinFeeA()) &&
            Objects.isNull(protocols.getMinFeeB()) &&
            Objects.isNull(protocols.getMaxBlockSize()) &&
            Objects.isNull(protocols.getMaxTxSize()) &&
            Objects.isNull(protocols.getMaxBhSize()) &&
            Objects.isNull(protocols.getKeyDeposit()) &&
            Objects.isNull(protocols.getPoolDeposit()) &&
            Objects.isNull(protocols.getMaxEpoch()) &&
            Objects.isNull(protocols.getOptimalPoolCount()) &&
            Objects.isNull(protocols.getMinUtxoValue()) &&
            Objects.isNull(protocols.getMinPoolCost()) &&
            Objects.isNull(protocols.getMaxTxExMem()) &&
            Objects.isNull(protocols.getMaxTxExSteps()) &&
            Objects.isNull(protocols.getMaxBlockExMem()) &&
            Objects.isNull(protocols.getMaxBlockExSteps()) &&
            Objects.isNull(protocols.getMaxValSize()) &&
            Objects.isNull(protocols.getCoinsPerUtxoSize()) &&
            Objects.isNull(protocols.getInfluence()) &&
            Objects.isNull(protocols.getMonetaryExpandRate()) &&
            Objects.isNull(protocols.getTreasuryGrowthRate()) &&
            Objects.isNull(protocols.getDecentralisation()) &&
            Objects.isNull(protocols.getPriceMem()) &&
            Objects.isNull(protocols.getPriceStep()) &&
            Objects.isNull(protocols.getProtocolMajor()) &&
            Objects.isNull(protocols.getProtocolMinor()) &&
            Objects.isNull(protocols.getCollateralPercent()) &&
            Objects.isNull(protocols.getMaxCollateralInputs()) &&
            Objects.isNull(protocols.getEntropy()) &&
            Objects.isNull(protocols.getCostModel()))
        .forEach(paramProposal -> {

          if (Objects.isNull(protocols.getMinFeeA()) &&
              Objects.nonNull(paramProposal.getMinFeeA())) {
            protocols.setMinFeeA(getChangeProtocol(
                paramProposal.getMinFeeA(),
                txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMinFeeB()) &&
              Objects.nonNull(paramProposal.getMinFeeB())) {
            protocols.setMinFeeB(
                getChangeProtocol(
                    paramProposal.getMinFeeB(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxBlockSize()) &&
              Objects.nonNull(paramProposal.getMaxBlockSize())) {
            protocols.setMaxBlockSize(
                getChangeProtocol(
                    paramProposal.getMaxBlockSize(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxTxSize()) &&
              Objects.nonNull(paramProposal.getMaxTxSize())) {
            protocols.setMaxTxSize(
                getChangeProtocol(
                    paramProposal.getMaxTxSize(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxBhSize()) &&
              Objects.nonNull(paramProposal.getMaxBhSize())) {
            protocols.setMaxBhSize(
                getChangeProtocol(
                    paramProposal.getMaxBhSize(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getKeyDeposit()) &&
              Objects.nonNull(paramProposal.getKeyDeposit())) {
            protocols.setKeyDeposit(
                getChangeProtocol(
                    paramProposal.getKeyDeposit(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getPoolDeposit()) &&
              Objects.nonNull(paramProposal.getPoolDeposit())) {
            protocols.setPoolDeposit(
                getChangeProtocol(
                    paramProposal.getPoolDeposit(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxEpoch()) &&
              Objects.nonNull(paramProposal.getMaxEpoch())) {
            protocols.setMaxEpoch(
                getChangeProtocol(
                    paramProposal.getMaxEpoch(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getOptimalPoolCount()) &&
              Objects.nonNull(paramProposal.getOptimalPoolCount())) {
            protocols.setOptimalPoolCount(
                getChangeProtocol(
                    paramProposal.getOptimalPoolCount(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMinUtxoValue()) &&
              Objects.nonNull(paramProposal.getMinUtxoValue())) {
            protocols.setMinUtxoValue(
                getChangeProtocol(
                    paramProposal.getMinUtxoValue(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMinPoolCost()) &&
              Objects.nonNull(paramProposal.getMinPoolCost())) {
            protocols.setMinPoolCost(
                getChangeProtocol(
                    paramProposal.getMinPoolCost(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxTxExMem()) &&
              Objects.nonNull(paramProposal.getMaxTxExMem())) {
            protocols.setMaxTxExMem(
                getChangeProtocol(
                    paramProposal.getMaxTxExMem(),
                    txs.get(paramProposal.getTx())));
          }
          if (Objects.isNull(protocols.getMaxTxExSteps()) &&
              Objects.nonNull(paramProposal.getMaxTxExSteps())) {
            protocols.setMaxTxExSteps(
                getChangeProtocol(
                    paramProposal.getMaxTxExSteps(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxBlockExMem()) &&
              Objects.nonNull(paramProposal.getMaxBlockExMem())) {
            protocols.setMaxBlockExMem(
                getChangeProtocol(
                    paramProposal.getMaxBlockExMem(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxBlockExSteps()) &&
              Objects.nonNull(paramProposal.getMaxBlockExSteps())) {
            protocols.setMaxBlockExSteps(
                getChangeProtocol(
                    paramProposal.getMaxBlockExSteps(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxValSize()) &&
              Objects.nonNull(paramProposal.getMaxValSize())) {
            protocols.setMaxValSize(
                getChangeProtocol(
                    paramProposal.getMaxValSize(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getCoinsPerUtxoSize()) &&
              Objects.nonNull(paramProposal.getCoinsPerUtxoSize())) {
            protocols.setCoinsPerUtxoSize(
                getChangeProtocol(
                    paramProposal.getCoinsPerUtxoSize(),
                    txs.get(paramProposal.getTx())));
          }
          if (Objects.isNull(protocols.getInfluence()) &&
              Objects.nonNull(paramProposal.getInfluence())) {
            protocols.setInfluence(
                getChangeProtocol(
                    paramProposal.getInfluence(),
                    txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMonetaryExpandRate()) &&
              Objects.nonNull(paramProposal.getMonetaryExpandRate())) {
            protocols.setMonetaryExpandRate(getChangeProtocol(
                paramProposal.getMonetaryExpandRate(),
                txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getTreasuryGrowthRate()) &&
              Objects.nonNull(paramProposal.getTreasuryGrowthRate())) {
            protocols.setTreasuryGrowthRate(getChangeProtocol(
                paramProposal.getTreasuryGrowthRate(),
                txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getDecentralisation()) &&
              Objects.nonNull(paramProposal.getDecentralisation())) {
            protocols.setDecentralisation(
                getChangeProtocol(paramProposal.getDecentralisation(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getPriceMem()) &&
              Objects.nonNull(paramProposal.getPriceMem())) {
            protocols.setPriceMem(
                getChangeProtocol(paramProposal.getPriceMem(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getPriceStep()) &&
              Objects.nonNull(paramProposal.getPriceStep())) {
            protocols.setPriceStep(
                getChangeProtocol(paramProposal.getPriceStep(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getProtocolMajor()) &&
              Objects.nonNull(paramProposal.getProtocolMajor())) {
            protocols.setProtocolMajor(
                getChangeProtocol(paramProposal.getProtocolMajor(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getProtocolMinor()) &&
              Objects.nonNull(paramProposal.getProtocolMinor())) {
            protocols.setProtocolMinor(
                getChangeProtocol(paramProposal.getProtocolMinor(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getCollateralPercent()) &&
              Objects.nonNull(paramProposal.getCollateralPercent())) {
            protocols.setCollateralPercent(
                getChangeProtocol(paramProposal.getCollateralPercent(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getMaxCollateralInputs()) &&
              Objects.nonNull(paramProposal.getMaxCollateralInputs())) {
            protocols.setMaxCollateralInputs(
                getChangeProtocol(paramProposal.getMaxCollateralInputs(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(protocols.getEntropy()) &&
              Objects.nonNull(paramProposal.getEntropy())) {
            protocols.setEntropy(
                getChangeProtocol(paramProposal.getEntropy(),
                                  txs.get(paramProposal.getTx())));
          }

          if (Objects.isNull(paramProposal.getCostModel())) {
            return;
          }

          if (Objects.isNull(protocols.getCostModel())) {
            protocols.setCostModel(
                getChangeCostModelProtocol(paramProposal.getCostModel(),
                                           txs.get(paramProposal.getTx())));
          }
        });
  }

  private Protocols mapProtocols(EpochParam epochParam) {
    var protocols = Protocols.builder()
        .minFeeA(getChangeProtocol(epochParam.getMinFeeA()))
        .minFeeB(getChangeProtocol(epochParam.getMinFeeB()))
        .maxBlockSize(getChangeProtocol(epochParam.getMaxBlockSize()))
        .maxTxSize(getChangeProtocol(epochParam.getMaxTxSize()))
        .maxBhSize(getChangeProtocol(epochParam.getMaxBhSize()))
        .keyDeposit(getChangeProtocol(epochParam.getKeyDeposit()))
        .poolDeposit(getChangeProtocol(epochParam.getPoolDeposit()))
        .maxEpoch(getChangeProtocol(epochParam.getMaxEpoch()))
        .optimalPoolCount(getChangeProtocol(epochParam.getOptimalPoolCount()))
        .influence(getChangeProtocol(epochParam.getInfluence()))
        .monetaryExpandRate(getChangeProtocol(epochParam.getMonetaryExpandRate()))
        .treasuryGrowthRate(getChangeProtocol(epochParam.getTreasuryGrowthRate()))
        .decentralisation(getChangeProtocol(epochParam.getDecentralisation()))
        .entropy(getChangeProtocol(epochParam.getExtraEntropy()))
        .protocolMajor(getChangeProtocol(epochParam.getProtocolMajor()))
        .protocolMinor(getChangeProtocol(epochParam.getProtocolMinor()))
        .minUtxoValue(getChangeProtocol(
            epochParam.getMinUtxoValue()))
        .minPoolCost(getChangeProtocol(
            epochParam.getMinPoolCost()))
        .priceMem(getChangeProtocol(
            epochParam.getPriceMem()))
        .priceStep(getChangeProtocol(
            epochParam.getPriceStep()))
        .maxTxExMem(getChangeProtocol(
            epochParam.getMaxTxExMem()))
        .maxTxExSteps(getChangeProtocol(
            epochParam.getMaxTxExSteps()))
        .maxBlockExMem(getChangeProtocol(
            epochParam.getMaxBlockExMem()))
        .maxBlockExSteps(getChangeProtocol(
            epochParam.getMaxBlockExSteps()))
        .maxValSize(getChangeProtocol(
            epochParam.getMaxValSize()))
        .collateralPercent(getChangeProtocol(
            epochParam.getCollateralPercent()))
        .maxCollateralInputs(getChangeProtocol(
            epochParam.getMaxCollateralInputs()))
        .coinsPerUtxoSize(getChangeProtocol(
            epochParam.getCoinsPerUtxoSize()))
        .endEpoch(epochParam.getEpochNo())
        .startEpoch(epochParam.getEpochNo())
        .build();
    if (Objects.nonNull(epochParam.getCostModel())) {
      protocols.setCostModel(getChangeProtocol(
          epochParam.getCostModel().getCosts()));
    }

    return protocols;
  }

  private void fillMissingProtocolField(Protocols protocols, EpochParam epochParam) {
    if (Objects.isNull(protocols.getMinFeeA())) {
      protocols.setMinFeeA(getChangeProtocol(
          epochParam.getMinFeeA()));
    }

    if (Objects.isNull(protocols.getMinFeeB())) {
      protocols.setMinFeeB(
          getChangeProtocol(
              epochParam.getMinFeeB()));
    }

    if (Objects.isNull(protocols.getMaxBlockSize())) {
      protocols.setMaxBlockSize(
          getChangeProtocol(
              epochParam.getMaxBlockSize()
          ));
    }

    if (Objects.isNull(protocols.getMaxTxSize())) {
      protocols.setMaxTxSize(
          getChangeProtocol(
              epochParam.getMaxTxSize()
          ));
    }

    if (Objects.isNull(protocols.getMaxBhSize())) {
      protocols.setMaxBhSize(
          getChangeProtocol(
              epochParam.getMaxBhSize()
          ));
    }

    if (Objects.isNull(protocols.getKeyDeposit())) {
      protocols.setKeyDeposit(
          getChangeProtocol(
              epochParam.getKeyDeposit()
          ));
    }

    if (Objects.isNull(protocols.getPoolDeposit())) {
      protocols.setPoolDeposit(
          getChangeProtocol(
              epochParam.getPoolDeposit()
          ));
    }

    if (Objects.isNull(protocols.getMaxEpoch())) {
      protocols.setMaxEpoch(
          getChangeProtocol(
              epochParam.getMaxEpoch()
          ));
    }

    if (Objects.isNull(protocols.getOptimalPoolCount())) {
      protocols.setOptimalPoolCount(
          getChangeProtocol(
              epochParam.getOptimalPoolCount()
          ));
    }

    if (Objects.isNull(protocols.getMinUtxoValue())) {
      protocols.setMinUtxoValue(
          getChangeProtocol(
              epochParam.getMinUtxoValue()
          ));
    }

    if (Objects.isNull(protocols.getMinPoolCost())) {
      protocols.setMinPoolCost(
          getChangeProtocol(
              epochParam.getMinPoolCost()
          ));
    }

    if (Objects.isNull(protocols.getMaxTxExMem())) {
      protocols.setMaxTxExMem(
          getChangeProtocol(
              epochParam.getMaxTxExMem()
          ));
    }
    if (Objects.isNull(protocols.getMaxTxExSteps())) {
      protocols.setMaxTxExSteps(
          getChangeProtocol(
              epochParam.getMaxTxExSteps()
          ));
    }

    if (Objects.isNull(protocols.getMaxBlockExMem())) {
      protocols.setMaxBlockExMem(
          getChangeProtocol(
              epochParam.getMaxBlockExMem()
          ));
    }

    if (Objects.isNull(protocols.getMaxBlockExSteps())) {
      protocols.setMaxBlockExSteps(
          getChangeProtocol(
              epochParam.getMaxBlockExSteps()
          ));
    }

    if (Objects.isNull(protocols.getMaxValSize())) {
      protocols.setMaxValSize(
          getChangeProtocol(
              epochParam.getMaxValSize()
          ));
    }

    if (Objects.isNull(protocols.getCoinsPerUtxoSize())) {
      protocols.setCoinsPerUtxoSize(
          getChangeProtocol(
              epochParam.getCoinsPerUtxoSize()
          ));
    }
    if (Objects.isNull(protocols.getInfluence())) {
      protocols.setInfluence(
          getChangeProtocol(
              epochParam.getInfluence()
          ));
    }

    if (Objects.isNull(protocols.getMonetaryExpandRate())) {
      protocols.setMonetaryExpandRate(getChangeProtocol(
          epochParam.getMonetaryExpandRate()
      ));
    }

    if (Objects.isNull(protocols.getTreasuryGrowthRate())) {
      protocols.setTreasuryGrowthRate(getChangeProtocol(
          epochParam.getTreasuryGrowthRate()
      ));
    }

    if (Objects.isNull(protocols.getDecentralisation())) {
      protocols.setDecentralisation(
          getChangeProtocol(epochParam.getDecentralisation()
          ));
    }

    if (Objects.isNull(protocols.getPriceMem())) {
      protocols.setPriceMem(
          getChangeProtocol(epochParam.getPriceMem()
          ));
    }

    if (Objects.isNull(protocols.getPriceStep())) {
      protocols.setPriceStep(
          getChangeProtocol(epochParam.getPriceStep()
          ));
    }

    if (Objects.isNull(protocols.getProtocolMajor())) {
      protocols.setProtocolMajor(
          getChangeProtocol(epochParam.getProtocolMajor()
          ));
    }

    if (Objects.isNull(protocols.getProtocolMinor())) {
      protocols.setProtocolMinor(
          getChangeProtocol(epochParam.getProtocolMinor()
          ));
    }

    if (Objects.isNull(protocols.getCollateralPercent())) {
      protocols.setCollateralPercent(
          getChangeProtocol(epochParam.getCollateralPercent()
          ));
    }

    if (Objects.isNull(protocols.getMaxCollateralInputs())) {
      protocols.setMaxCollateralInputs(
          getChangeProtocol(epochParam.getMaxCollateralInputs()
          ));
    }

    if (Objects.isNull(protocols.getEntropy())) {
      protocols.setEntropy(
          getChangeProtocol(epochParam.getExtraEntropy()
          ));
    }

    if (Objects.isNull(epochParam.getCostModel())) {
      return;
    }

    if (Objects.isNull(protocols.getCostModel()) && Objects.nonNull(epochParam.getCostModel())) {
      protocols.setCostModel(
          getChangeCostModelProtocol(epochParam.getCostModel().getCosts()));
    }
  }

  private void fillFixedProtocolField(Map<ProtocolType, Object> fixedProtocols,
                                      Protocols protocols) {

    fixedProtocols.keySet().forEach(fieldName -> {
      Object object = fixedProtocols.get(fieldName);
      switch (fieldName) {
        case MIN_FEE_A:
          protocols.setMinFeeA(getChangeProtocol(object));
          break;
        case MIN_FEE_B:
          protocols.setMinFeeA(getChangeProtocol(object));
          break;
        case MAX_BLOCK_SIZE:
          protocols.setMaxBlockSize(getChangeProtocol(object));
          break;
        case MAX_TX_SIZE:
          protocols.setMaxTxSize(getChangeProtocol(object));
          break;
        case MAX_BH_SIZE:
          protocols.setMaxBhSize(getChangeProtocol(object));
          break;
        case KEY_DEPOSIT:
          protocols.setKeyDeposit(getChangeProtocol(object));
          break;
        case POOL_DEPOSIT:
          protocols.setPoolDeposit(getChangeProtocol(object));
          break;
        case MAX_EPOCH:
          protocols.setMaxEpoch(getChangeProtocol(object));
          break;
        case OPTIMAL_POOL_COUNT:
          protocols.setOptimalPoolCount(getChangeProtocol(object));
          break;
        case MIN_UTXO_VALUE:
          protocols.setMinUtxoValue(getChangeProtocol(object));
          break;
        case MIN_POOL_COST:
          protocols.setMinPoolCost(getChangeProtocol(object));
          break;
        case MAX_TX_EX_MEM:
          protocols.setMaxTxExMem(getChangeProtocol(object));
          break;
        case MAX_TX_EX_STEPS:
          protocols.setMaxTxExSteps(getChangeProtocol(object));
          break;
        case MAX_BLOCK_EX_MEM:
          protocols.setMaxBlockExMem(getChangeProtocol(object));
          break;
        case MAX_BLOCK_EX_STEPS:
          protocols.setMaxBlockExSteps(getChangeProtocol(object));
          break;
        case MAX_VAL_SIZE:
          protocols.setMaxValSize(getChangeProtocol(object));
          break;
        case COINS_PER_UTXO_SIZE:
          protocols.setCoinsPerUtxoSize(getChangeProtocol(object));
          break;
        case INFLUENCE:
          protocols.setInfluence(getChangeProtocol(object));
          break;
        case MONETARY_EXPAND_RATE:
          protocols.setMonetaryExpandRate(getChangeProtocol(object));
          break;
        case TREASURY_GROWTH_RATE:
          protocols.setTreasuryGrowthRate(getChangeProtocol(object));
          break;
        case DECENTRALISATION:
          protocols.setDecentralisation(getChangeProtocol(object));
          break;
        case PRICE_MEM:
          protocols.setPriceMem(getChangeProtocol(object));
          break;
        case PRICE_STEP:
          protocols.setPriceStep(getChangeProtocol(object));
          break;
        case PROTOCOL_MAJOR:
          protocols.setProtocolMajor(getChangeProtocol(object));
          break;
        case PROTOCOL_MINOR:
          protocols.setProtocolMinor(getChangeProtocol(object));
          break;
        case COLLATERAL_PERCENT:
          protocols.setCollateralPercent(getChangeProtocol(object));
          break;
        case MAX_COLLATERAL_INPUTS:
          protocols.setMaxCollateralInputs(getChangeProtocol(object));
          break;
        case ENTROPY:
          protocols.setEntropy(getChangeProtocol(object));
          break;
        case COST_MODEL:
          CostModel costModel = (CostModel) object;
          protocols.setCostModel(getChangeProtocol(costModel.getCosts()));
          break;
        default:
          throw new BusinessException(BusinessCode.PROTOCOL_FIELD_NOT_FOUND);
      }
    });
  }

  @PostConstruct
  void setup() {
    paramProtocolMethod = new HashMap<>();
    Field[] fields = EpochParam.class.getDeclaredFields();
    Method[] methods = EpochParam.class.getDeclaredMethods();
    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (Field field : fields) {
      Method methodUsed = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.getName().toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          })
          .findFirst()
          .orElse(null); // Method null is ok, because we not use it anyway
      if (Objects.nonNull(methodUsed) &&
          fieldNames.contains(field.getName())) {
        paramProtocolMethod.put(ProtocolType.valueStringOf(field.getName()), methodUsed);
      }
    }
  }
}
