package org.cardanofoundation.explorer.api.service.impl;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import jakarta.annotation.PostConstruct;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolStatus;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.model.response.protocol.EpochChange;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
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

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolParamServiceImpl implements ProtocolParamService {

  public static final String EPOCH_CHANGE_FIELD = "epochchanges";
  public static final String STATUS = "status";
  final ParamProposalRepository paramProposalRepository;
  final EpochParamRepository epochParamRepository;
  final TxRepository txRepository;
  final CostModelRepository costModelRepository;
  final ProtocolMapper protocolMapper;

  @Value("${application.network}")
  String network;
  public static final String GET = "get";
  Map<ProtocolType, Method> paramProtocolMethods;
  Map<String, Method> historyMethods;

  /**
   * Find history change of protocol parameters
   *
   * @return histories change
   */
  @Override
  public HistoriesProtocol getHistoryProtocolParameters() {
    // find all param proposal change, and take the last change
    List<ParamHistory> historiesChange = paramProposalRepository.findProtocolsChange();
    // find all epoch param group there in to map of key:epoch value:epoch_param
    Map<Integer, EpochParam> epochParams = epochParamRepository.findAll().stream()
        .collect(Collectors.toMap(EpochParam::getEpochNo, Function.identity(), (a, b) -> a));
    // group by epoch
    Map<Integer, List<ParamHistory>> historiesChangeByEpoch = historiesChange
        .parallelStream()
        .collect(Collectors.groupingBy(ParamHistory::getEpochNo, Collectors.toList()));
    // find all transaction update protocol param
    Map<Long, Tx> txs = txRepository.findByIdIn(
            historiesChange.stream().map(ParamHistory::getTx).toList())
        .parallelStream().collect(Collectors.toMap(Tx::getId, Function.identity()));
    // group the un process to map
    Map<Integer, Protocols> unprocessedProtocols = epochParams.keySet()
        .stream()
        .sorted(Integer::compareTo)
        .map(epoch -> {
          Protocols protocols = getEpochProtocol(epoch);
          List<ParamHistory> protocolHistories = historiesChangeByEpoch.get(
              epoch - BigInteger.ONE.intValue());

          if (ObjectUtils.isEmpty(protocolHistories)) {
            return protocols;
          }

          Protocols protocolsChange = getEpochProtocol(
              protocolHistories.get(BigInteger.ZERO.intValue()).getEpochNo()
                  + BigInteger.ONE.intValue());
          getProtocolChangeInOneEpoch(protocolHistories, txs, protocolsChange);

          if (Objects.equals(protocols, protocolsChange)) {
            return protocols;
          }
          return protocolsChange;
        })
        .collect(Collectors
            .toMap(protocols -> protocols.getEpochChange().getStartEpoch(), Function.identity()));

    List<Protocols> processProtocols = new ArrayList<>();

    if (ObjectUtils.isEmpty(epochParams.keySet())) {
      return new HistoriesProtocol(); // return empty if epoch param is empty
    }

    final Integer min = epochParams.keySet().stream().min(Integer::compareTo)
        .orElse(BigInteger.ZERO.intValue());
    final Integer max = historiesChangeByEpoch.keySet().stream().max(Integer::compareTo)
        .orElse(BigInteger.ZERO.intValue()) + BigInteger.TWO.intValue();

    AtomicReference<Protocols> currentMarkProtocols = new AtomicReference<>(
        unprocessedProtocols.get(max - BigInteger.ONE.intValue()));

    // check unprocessedProtocols data
    IntStream.range(min, max)
        .boxed()
        .sorted(Collections.reverseOrder())
        .forEach(epoch -> {

          Protocols markProtocol = currentMarkProtocols.get();
          Protocols currentProtocol = unprocessedProtocols.get(epoch);

          if (Objects.isNull(currentProtocol)) {
            currentProtocol = mapProtocols(epochParams.get(epoch));
          }

          if (Objects.equals(markProtocol, currentProtocol)) {
            markProtocol.getEpochChange().setEndEpoch(epoch);

            if (min.equals(epoch)) {
              fillMissingProtocolField(markProtocol, epochParams.get(epoch));
              processProtocols.add(markProtocol);
            }
            return;
          }

          fillMissingProtocolField(markProtocol, epochParams.get(epoch));

          processProtocols.add(currentMarkProtocols.get());
          currentMarkProtocols.set(currentProtocol);

          if (min.equals(epoch)) {
            fillMissingProtocolField(currentProtocol, epochParams.get(epoch));
            processProtocols.add(currentMarkProtocols.get());
          }
        });

    HistoriesProtocol historiesProtocol = protocolMapper.mapProtocolsToHistoriesProtocol(
        processProtocols);
    handleHistoriesChange(historiesProtocol);
    return historiesProtocol;
  }

  /**
   * Find latest protocol param have changed
   *
   * @return
   */
  @Override
  public Protocols getLatestChange() {
    Integer epoch = paramProposalRepository.findMaxEpoch();

    if (Objects.isNull(epoch)) {
      return new Protocols();
    }

    List<ParamHistory> paramHistories = paramProposalRepository
        .findEpochProtocolsChange(epoch)
        .stream()
        .sorted((proposalOld, proposalNew)
            -> proposalNew.getTx().compareTo(proposalOld.getTx()))
        .toList();

    if (CollectionUtils.isEmpty(paramHistories)) {
      return new Protocols();
    }

    Optional<EpochParam> epochParamOptional = epochParamRepository.findEpochParamByEpochNo(
        epoch + BigInteger.ONE.intValue());

    return epochParamOptional
        .map(epochParam -> {

          Map<Long, Tx> txs = txRepository.findByIdIn(
                  paramHistories.stream().map(ParamHistory::getTx).toList())
              .parallelStream().collect(Collectors.toMap(Tx::getId, Function.identity()));

          Protocols epochChange = getEpochProtocol(
              paramHistories.get(BigInteger.ZERO.intValue()).getEpochNo());
          getProtocolChangeInOneEpoch(paramHistories, txs, epochChange);

          epochChange.getEpochChange().setStartEpoch(epoch + BigInteger.ONE.intValue());
          fillMissingProtocolField(epochChange, epochParam);
          return epochChange;
        }).orElse(Protocols.builder().build());
  }

  @Override
  public FixedProtocol getFixedProtocols() {
    FixedProtocol fixedProtocol = null;
    try {
      return protocolMapper.mapFixedProtocol(network);
    } catch (JsonProcessingException e) {
      e.printStackTrace();
    }

    log.error("{} network is not prepared ", network);

    return fixedProtocol;
  }

  public static ProtocolHistory getChangeCostModelProtocol(String cost) {

    return ProtocolHistory.builder()
        .value(cost)
        .time(null)
        .transactionHash(null)
        .build();
  }

  public static ProtocolHistory getChangeProtocol(Object currentProtocol, Tx tx,
                                                  AtomicReference<Date> timeChange) {
    var utcOffsetDateTime = OffsetDateTime.ofInstant(tx.getBlock().getTime().toInstant(),
        ZoneOffset.UTC);
    timeChange.set(Timestamp.valueOf(utcOffsetDateTime.toLocalDateTime()));
    return ProtocolHistory.builder()
        .value(currentProtocol)
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build();
  }

  public static ProtocolHistory getChangeProtocol(Object object) {
    return ProtocolHistory.builder()
        .value(object)
        .time(null)
        .transactionHash(null)
        .build();
  }

  public static Protocols mapProtocols(EpochParam epochParam) {
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
        .epochChange(EpochChange.builder()
            .endEpoch(epochParam.getEpochNo())
            .startEpoch(epochParam.getEpochNo())
            .build())
        .build();
    if (Objects.nonNull(epochParam.getCostModel())) {
      protocols.setCostModel(getChangeProtocol(
          epochParam.getCostModel().getCosts()));
    } else {
      protocols.setCostModel(ProtocolHistory.builder().build());
    }

    return protocols;
  }

  private void mapProtocols(List<ParamHistory> paramProposals, Protocols protocols,
                            Map<Long, Tx> txs) {
    paramProposals
        .stream()
        .sorted(Comparator.comparing(ParamHistory::getEpochNo).reversed().thenComparing(ParamHistory::getTx))
        .forEach(paramProposal -> {
          AtomicReference<Date> timeChange = new AtomicReference<>(null);
          if (Objects.nonNull(paramProposal.getMinFeeA())) {
              if(protocols.getMinFeeA() == null || !protocols.getMinFeeA().getValue().equals(paramProposal.getMinFeeA())) {
                  protocols.setMinFeeA(getChangeProtocol(
                          paramProposal.getMinFeeA(),
                          txs.get(paramProposal.getTx()), timeChange));
              }

          }

          if (Objects.nonNull(paramProposal.getMinFeeB())) {
              if(protocols.getMinFeeB() == null || !protocols.getMinFeeB().getValue().equals(paramProposal.getMinFeeB())) {
                  protocols.setMinFeeB(
                          getChangeProtocol(
                                  paramProposal.getMinFeeB(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }

          }

          if (Objects.nonNull(paramProposal.getMaxBlockSize())) {

              if(Objects.isNull(protocols.getMaxBlockSize()) || !protocols.getMaxBlockSize().getValue().equals(paramProposal.getMaxBlockSize())) {
                  protocols.setMaxBlockSize(
                          getChangeProtocol(
                                  paramProposal.getMaxBlockSize(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxTxSize())) {

              if(Objects.isNull(protocols.getMaxTxSize()) || !protocols.getMaxTxSize().getValue().equals(paramProposal.getMaxTxSize())) {
                  protocols.setMaxTxSize(
                          getChangeProtocol(
                                  paramProposal.getMaxTxSize(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxBhSize())) {

              if(Objects.isNull(protocols.getMaxBhSize()) || !protocols.getMaxBhSize().getValue().equals(paramProposal.getMaxBhSize())) {
                  protocols.setMaxBhSize(
                          getChangeProtocol(
                                  paramProposal.getMaxBhSize(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getKeyDeposit())) {

              if(Objects.isNull(protocols.getKeyDeposit()) || !protocols.getKeyDeposit().getValue().equals(paramProposal.getKeyDeposit())) {
                  protocols.setKeyDeposit(
                          getChangeProtocol(
                                  paramProposal.getKeyDeposit(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getPoolDeposit())) {

              if(Objects.isNull(protocols.getPoolDeposit()) || !protocols.getPoolDeposit().getValue().equals(paramProposal.getPoolDeposit())) {
                  protocols.setPoolDeposit(
                          getChangeProtocol(
                                  paramProposal.getPoolDeposit(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxEpoch())) {

              if(Objects.isNull(protocols.getMaxEpoch()) || !protocols.getMaxEpoch().getValue().equals(paramProposal.getMaxEpoch())) {
                  protocols.setMaxEpoch(
                          getChangeProtocol(
                                  paramProposal.getMaxEpoch(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getOptimalPoolCount())) {

              if(Objects.isNull(protocols.getOptimalPoolCount()) || !protocols.getOptimalPoolCount().getValue().equals(paramProposal.getOptimalPoolCount())) {
                  protocols.setOptimalPoolCount(
                          getChangeProtocol(
                                  paramProposal.getOptimalPoolCount(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMinUtxoValue())) {

              if(Objects.isNull(protocols.getMinUtxoValue()) || !protocols.getMinUtxoValue().getValue().equals(paramProposal.getMinUtxoValue())) {
                  protocols.setMinUtxoValue(
                          getChangeProtocol(
                                  paramProposal.getMinUtxoValue(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMinPoolCost())) {

              if(Objects.isNull(protocols.getMinPoolCost()) || !protocols.getMinPoolCost().getValue().equals(paramProposal.getMinPoolCost())) {
                  protocols.setMinPoolCost(
                          getChangeProtocol(
                                  paramProposal.getMinPoolCost(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxTxExMem())) {

              if(Objects.isNull(protocols.getMaxTxExMem()) || !protocols.getMaxTxExMem().getValue().equals(paramProposal.getMaxTxExMem())) {
                  protocols.setMaxTxExMem(
                          getChangeProtocol(
                                  paramProposal.getMaxTxExMem(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }
          if (Objects.nonNull(paramProposal.getMaxTxExSteps())) {

              if(Objects.isNull(protocols.getMaxTxExSteps()) || !protocols.getMaxTxExSteps().getValue().equals(paramProposal.getMaxTxExSteps())) {
                  protocols.setMaxTxExSteps(
                          getChangeProtocol(
                                  paramProposal.getMaxTxExSteps(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxBlockExMem())) {

              if(Objects.isNull(protocols.getMaxBlockExMem()) || !protocols.getMaxBlockExMem().getValue().equals(paramProposal.getMaxBlockExMem())) {
                  protocols.setMaxBlockExMem(
                          getChangeProtocol(
                                  paramProposal.getMaxBlockExMem(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxBlockExSteps())) {

              if(Objects.isNull(protocols.getMaxBlockExSteps()) || !protocols.getMaxBlockExSteps().getValue().equals(paramProposal.getMaxBlockExSteps())) {
                  protocols.setMaxBlockExSteps(
                          getChangeProtocol(
                                  paramProposal.getMaxBlockExSteps(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxValSize())) {

              if(Objects.isNull(protocols.getMaxValSize()) || !protocols.getMaxValSize().getValue().equals(paramProposal.getMaxValSize())) {
                  protocols.setMaxValSize(
                          getChangeProtocol(
                                  paramProposal.getMaxValSize(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getCoinsPerUtxoSize())) {

              if(Objects.isNull(protocols.getCoinsPerUtxoSize()) || !protocols.getCoinsPerUtxoSize().getValue().equals(paramProposal.getCoinsPerUtxoSize())) {
                  protocols.setCoinsPerUtxoSize(
                          getChangeProtocol(
                                  paramProposal.getCoinsPerUtxoSize(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }
          if (Objects.nonNull(paramProposal.getInfluence())) {

              if(Objects.isNull(protocols.getInfluence()) || !protocols.getInfluence().getValue().equals(paramProposal.getInfluence())) {
                  protocols.setInfluence(
                          getChangeProtocol(
                                  paramProposal.getInfluence(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMonetaryExpandRate())) {

              if(Objects.isNull(protocols.getMonetaryExpandRate()) || !protocols.getMonetaryExpandRate().getValue().equals(paramProposal.getMonetaryExpandRate())) {
                  protocols.setMonetaryExpandRate(getChangeProtocol(
                          paramProposal.getMonetaryExpandRate(),
                          txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getTreasuryGrowthRate())) {

              if(Objects.isNull(protocols.getTreasuryGrowthRate()) || !protocols.getTreasuryGrowthRate().getValue().equals(paramProposal.getTreasuryGrowthRate())) {
                  protocols.setTreasuryGrowthRate(getChangeProtocol(
                          paramProposal.getTreasuryGrowthRate(),
                          txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getDecentralisation())) {
              if(Objects.isNull(protocols.getDecentralisation()) || !protocols.getDecentralisation().getValue().equals(paramProposal.getDecentralisation())) {
                  protocols.setDecentralisation(
                          getChangeProtocol(paramProposal.getDecentralisation(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }

          }

          if (Objects.nonNull(paramProposal.getPriceMem())) {

              if(Objects.isNull(protocols.getPriceMem()) || !protocols.getPriceMem().getValue().equals(paramProposal.getPriceMem())) {
                  protocols.setPriceMem(
                          getChangeProtocol(paramProposal.getPriceMem(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getPriceStep())) {

              if(Objects.isNull(protocols.getPriceStep()) || !protocols.getPriceStep().getValue().equals(paramProposal.getPriceStep())) {
                  protocols.setPriceStep(
                          getChangeProtocol(paramProposal.getPriceStep(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getProtocolMajor())) {

              if(Objects.isNull(protocols.getProtocolMajor()) || !protocols.getProtocolMajor().getValue().equals(paramProposal.getProtocolMajor())) {
                  protocols.setProtocolMajor(
                          getChangeProtocol(paramProposal.getProtocolMajor(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getProtocolMinor())) {

              if(Objects.isNull(protocols.getProtocolMinor()) || !protocols.getProtocolMinor().getValue().equals(paramProposal.getProtocolMinor())) {
                  protocols.setProtocolMinor(
                          getChangeProtocol(paramProposal.getProtocolMinor(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getCollateralPercent())) {

              if(Objects.isNull(protocols.getCollateralPercent()) || !protocols.getCollateralPercent().getValue().equals(paramProposal.getCollateralPercent())) {
                  protocols.setCollateralPercent(
                          getChangeProtocol(paramProposal.getCollateralPercent(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getMaxCollateralInputs())) {

              if(Objects.isNull(protocols.getMaxCollateralInputs()) || !protocols.getMaxCollateralInputs().getValue().equals(paramProposal.getMaxCollateralInputs())) {
                  protocols.setMaxCollateralInputs(
                          getChangeProtocol(paramProposal.getMaxCollateralInputs(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getEntropy())) {

              if(Objects.isNull(protocols.getEntropy()) || !protocols.getEntropy().getValue().equals(paramProposal.getEntropy())) {
                  protocols.setEntropy(
                          getChangeProtocol(paramProposal.getEntropy(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(paramProposal.getCostModel())) {


              if(Objects.isNull(protocols.getCostModel()) || !protocols.getCostModel().getValue().equals(paramProposal.getCostModel())) {
                  protocols.setCostModel(
                          getChangeCostModelProtocol(paramProposal.getCostModel(),
                                  txs.get(paramProposal.getTx()), timeChange));
              }
          }

          if (Objects.nonNull(timeChange.get())) {
            if (Objects.isNull(protocols.getTimestamp())) {
              protocols.setTimestamp(timeChange.get());
              return;
            }

            if (protocols.getTimestamp().compareTo(timeChange.get()) < BigInteger.TEN.intValue()) {
              protocols.setTimestamp(timeChange.get());
            }
          }

        });
  }

  private Protocols getEpochProtocol(Integer epochNo) {
    return Protocols.builder()
        .epochChange(EpochChange.builder()
            .startEpoch(epochNo)
            .endEpoch(epochNo)
            .build())
        .build();
  }

  private void getProtocolChangeInOneEpoch(List<ParamHistory> paramHistories,
                                           Map<Long, Tx> txs, Protocols protocols) {
    mapProtocols(paramHistories, protocols, txs);
  }

  private ProtocolHistory getChangeCostModelProtocol(Long costModelId,
                                                     Tx tx, AtomicReference<Date> timeChange) {
    Optional<CostModel> costModel = costModelRepository.findById(costModelId);
    var utcOffsetDateTime = OffsetDateTime.ofInstant(tx.getBlock().getTime().toInstant(),
        ZoneOffset.UTC);
    timeChange.set(Timestamp.valueOf(utcOffsetDateTime.toLocalDateTime()));
    return costModel.map(model -> ProtocolHistory.builder()
        .value(model.getCosts())
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build()).orElse(null);
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

    if (Objects.isNull(protocols.getCostModel())) {
      if (Objects.nonNull(epochParam.getCostModel())) {
        protocols.setCostModel(
            getChangeCostModelProtocol(epochParam.getCostModel().getCosts()));
        return;
      }
      protocols.setCostModel(
          getChangeProtocol(epochParam.getCostModel()));

    }
  }

  private void handleHistoriesChange(HistoriesProtocol historiesProtocol) {
    historyMethods.values()
        //.parallelStream()
        .forEach(
            method -> {
              try {
                log.debug("method {}", method.getName());
                handleHistoryStatus((List<ProtocolHistory>) method.invoke(historiesProtocol));
              } catch (Exception e) {
                log.error(e.getMessage());
                log.error(e.getLocalizedMessage());
              }
            }
        );
  }

  private void handleHistoryStatus(List<ProtocolHistory> protocolHistories) {
    int size = protocolHistories.size() - BigInteger.ONE.intValue();

    IntStream.range(BigInteger.ZERO.intValue(), size)
        .forEach(index -> {
          final ProtocolHistory nextProtocolHistory =
              protocolHistories.get(index + BigInteger.ONE.intValue());

          final ProtocolHistory currentProtocolHistory = protocolHistories.get(index);
          if (Objects.isNull(nextProtocolHistory)) {

            if (Objects.nonNull(currentProtocolHistory.getValue())) {
              currentProtocolHistory.setStatus(ProtocolStatus.ADDED);
            }
            return;
          }

          if (Objects.isNull(currentProtocolHistory.getValue())) {
            currentProtocolHistory.setStatus(ProtocolStatus.NOT_EXIST);
            return;
          }

          if (Objects.isNull(nextProtocolHistory.getValue())) {
            currentProtocolHistory.setStatus(ProtocolStatus.ADDED);
            return;
          }

          if (currentProtocolHistory.getValue().hashCode() != nextProtocolHistory.getValue()
              .hashCode() && Objects.nonNull(currentProtocolHistory.getTransactionHash())) {
            currentProtocolHistory.setStatus(ProtocolStatus.UPDATED);
            return;
          }
          currentProtocolHistory.setStatus(ProtocolStatus.NOT_CHANGE);
        });

    ProtocolHistory lastProtocol = protocolHistories.get(size);
    if (Objects.isNull(lastProtocol.getValue())) {
      lastProtocol.setStatus(ProtocolStatus.NOT_EXIST);
      return;
    }
    lastProtocol.setStatus(ProtocolStatus.ADDED);
  }

  private void setMapParamProtocolMethods() {
    paramProtocolMethods = new HashMap<>();
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
        paramProtocolMethods.put(ProtocolType.valueStringOf(field.getName()), methodUsed);
      }
    }
  }

  private void setMapParamHistory() {
    historyMethods = new HashMap<>();
    Field[] fields = HistoriesProtocol.class.getDeclaredFields();
    Method[] methods = HistoriesProtocol.class.getDeclaredMethods();

    for (Field field : fields) {
      Method methodUsed = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.getName().toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) &&
                methodLowerCase.contains(GET) &&
                !methodLowerCase.contains(EPOCH_CHANGE_FIELD) &&
                !methodLowerCase.contains(STATUS);
          })
          .findFirst()
          .orElse(null); // Method null is ok, because we not use it anyway
      if (Objects.nonNull(methodUsed)) {
        historyMethods.put(field.getName(), methodUsed);
      }
    }
  }

  @PostConstruct
  public void setup() {
    setMapParamProtocolMethods();
    setMapParamHistory();
  }
}
