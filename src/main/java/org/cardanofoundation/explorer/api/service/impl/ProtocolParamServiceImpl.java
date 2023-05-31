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
import java.util.EnumMap;
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
import org.springframework.data.util.Pair;
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
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam_;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolParamServiceImpl implements ProtocolParamService {

  public static final String EPOCH_CHANGE_FIELD = "epochchanges";
  public static final String STATUS = "status";
  public static final String SET = "set";
  final ParamProposalRepository paramProposalRepository;
  final EpochParamRepository epochParamRepository;
  final TxRepository txRepository;
  final CostModelRepository costModelRepository;
  final ProtocolMapper protocolMapper;

  @Value("${application.network}")
  String network;
  public static final String GET = "get";
  // key::ProtocolType, value::getter>
  Map<ProtocolType, Method> epochParamMethods;

  // key::ProtocolType, value::Pair<setter,getter>
  Map<ProtocolType, Pair<Method, Method>> protocolsMethods;
  //key::ProtocolType, value::getter
  Map<ProtocolType, Method> paramHistoryMethods;
  //key::ProtocolType, value::Pair<setter,getter>
  Map<ProtocolType,  Pair<Method, Method>> historiesProtocolMethods;
  //key::String, value::getter

  /**
   * Find history change of protocol parameters
   *
   * @return histories change
   */
  @Override
  public HistoriesProtocol getHistoryProtocolParameters(List<ProtocolType> protocolTypes) {
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
          getProtocolChangeInOneEpoch(protocolHistories, txs, protocolsChange, protocolTypes);

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
              fillMissingProtocolField(markProtocol, epochParams.get(epoch), protocolTypes);
              processProtocols.add(markProtocol);
            }
            return;
          }

          fillMissingProtocolField(markProtocol, epochParams.get(epoch), protocolTypes);

          processProtocols.add(currentMarkProtocols.get());
          currentMarkProtocols.set(currentProtocol);

          if (min.equals(epoch)) {
            fillMissingProtocolField(currentProtocol, epochParams.get(epoch), protocolTypes);
            processProtocols.add(currentMarkProtocols.get());
          }
        });

    HistoriesProtocol historiesProtocol = protocolMapper.mapProtocolsToHistoriesProtocol(
        processProtocols, protocolsMethods , historiesProtocolMethods, protocolTypes);
    handleHistoriesChange(historiesProtocol, protocolTypes);
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
          getProtocolChangeInOneEpoch(paramHistories, txs, epochChange, ProtocolType.getAll());

          epochChange.getEpochChange().setStartEpoch(epoch + BigInteger.ONE.intValue());
          fillMissingProtocolField(epochChange, epochParam, ProtocolType.getAll());
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

    if (object instanceof CostModel costModel) {
      object = costModel.getCosts();
    }

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

  private Protocols getEpochProtocol(Integer epochNo) {
    return Protocols.builder()
        .epochChange(EpochChange.builder()
            .startEpoch(epochNo)
            .endEpoch(epochNo)
            .build())
        .build();
  }

  private void getProtocolChangeInOneEpoch(List<ParamHistory> paramHistories,
                                           Map<Long, Tx> txs, Protocols protocols,
                                           List<ProtocolType> protocolTypes) {
    mapProtocols(paramHistories, protocols, txs, protocolTypes);
  }

  private void mapProtocols(List<ParamHistory> paramProposals, Protocols protocols,
                            Map<Long, Tx> txs, List<ProtocolType> protocolTypes) {

    paramProposals
        .stream()
        .sorted(Comparator.comparing(ParamHistory::getEpochNo).reversed()
            .thenComparing(ParamHistory::getTx))
        .forEach(paramProposal -> {
          AtomicReference<Date> timeChange = new AtomicReference<>(null);
          // set value for protocols field
          paramHistoryMethods.entrySet().stream()
              .filter(entry -> protocolTypes.contains(entry.getKey()))
              .forEach(entry -> {
                try {
                  // get protocol
                  var protocolMethods = protocolsMethods.get(entry.getKey());
                  var protocolSet = protocolMethods.getFirst();
                  var protocolGet = protocolMethods.getSecond();
                  var protocolValue = (ProtocolHistory) protocolGet.invoke(protocols);

                  var paramProposalValue = entry.getValue().invoke(paramProposal);
                  // if value proposal not null
                  if (Objects.nonNull(paramProposalValue)) {
                    // insert if protocols is null or value change
                    if (Objects.requireNonNull(entry.getKey()) == ProtocolType.COST_MODEL) {
                      if (Objects.isNull(protocolValue) || !protocolValue.getCostModelId()
                          .equals(paramProposalValue)) {
                        protocols.setCostModel(
                            getChangeCostModelProtocol(paramProposal.getCostModel(),
                                txs.get(paramProposal.getTx()), timeChange));
                      }
                    } else {
                      if (Objects.isNull(protocolValue) || !protocolValue.getValue()
                          .equals(paramProposalValue)) {
                        protocolSet.invoke(protocols, getChangeProtocol(
                            paramProposalValue,
                            txs.get(paramProposal.getTx()), timeChange));
                      }
                    }
                  }
                } catch (Exception e) {
                  log.error(e.getMessage());
                  log.error(e.getLocalizedMessage());
                }
              });

          // update time change
          if (Objects.nonNull(timeChange.get())) {
            if (Objects.isNull(protocols.getTimestamp())) {
              protocols.setTimestamp(timeChange.get());
              return;
            }
            if (protocols.getTimestamp().compareTo(timeChange.get()) < BigInteger.ZERO.intValue()) {
              protocols.setTimestamp(timeChange.get());
            }
          }
        });
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
        .costModelId(costModelId)
        .build()).orElse(null);
  }

  private void fillMissingProtocolField(Protocols protocols, EpochParam epochParam,
                                        List<ProtocolType> protocolTypes) {
    protocolsMethods.entrySet()
        .stream()
        .filter(entry -> protocolTypes.contains(entry.getKey()))
        .forEach(entry -> {
          var methods = protocolsMethods.get(entry.getKey());
          try {
            if (Objects.isNull(entry.getValue().getSecond().invoke(protocols))) {
              methods.getFirst()
                  .invoke(protocols,
                      getChangeProtocol(epochParamMethods.get(entry.getKey()).invoke(epochParam)));
            }
          } catch (Exception e) {
            log.error(e.getMessage());
            log.error(e.getLocalizedMessage());
          }
        });
  }

  private void handleHistoriesChange(HistoriesProtocol historiesProtocol, List<ProtocolType> protocolTypes) {
    historiesProtocolMethods.entrySet()
        .stream().filter(entry -> protocolTypes.contains(entry.getKey()))
        .forEach(
            method -> {
              try {
                var historyProtocolGet =  method.getValue().getSecond();
                log.debug("method {}", method.getValue().getSecond().getName());
                handleHistoryStatus((List<ProtocolHistory>) historyProtocolGet.invoke(historiesProtocol));
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

          if (Objects.isNull(currentProtocolHistory)) {
            return;
          }

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

    if (Objects.nonNull(lastProtocol)) {
      if (Objects.isNull(lastProtocol.getValue())) {
        lastProtocol.setStatus(ProtocolStatus.NOT_EXIST);
        return;
      }
      lastProtocol.setStatus(ProtocolStatus.ADDED);
    }
  }

  private void setMapEpochParamMethods() {
    epochParamMethods = new EnumMap<>(ProtocolType.class);
    Field[] fields = EpochParam.class.getDeclaredFields();
    Method[] methods = EpochParam.class.getDeclaredMethods();
    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (Field field : fields) {
      Method methodUsed = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.getName().toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) &&
                methodLowerCase.contains(GET);
          })
          .findFirst()
          .orElse(null); // Method null is ok, because we not use it anyway

      if (Objects.nonNull(methodUsed) &&
          (fieldNames.contains(field.getName()) ||
              field.getName().equals(EpochParam_.EXTRA_ENTROPY)
          )) {
        epochParamMethods.put(ProtocolType.valueStringOf(field.getName()), methodUsed);
      }
    }
  }

  private void setProtocolMethodMap() {
    this.protocolsMethods = new EnumMap<>(ProtocolType.class);
    Method[] methods = Protocols.class.getDeclaredMethods();
    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (String field : fieldNames) {

      Method methodGet = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          }).findFirst()
          .orElse(null);

      Method methodSet = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(SET);
          }).findFirst()
          .orElse(null);

      if (Objects.nonNull(methodGet) && Objects.nonNull(methodSet)) {
        protocolsMethods.put(ProtocolType.valueStringOf(field), Pair.of(methodSet, methodGet));
      }
    }

  }

  private void setHistoriesProtocolMethods() {

    this.historiesProtocolMethods = new EnumMap<>(ProtocolType.class);

    Method[] methods = HistoriesProtocol.class.getDeclaredMethods();
    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (String field : fieldNames) {

      Method methodGet = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          }).findFirst()
          .orElse(null);

      Method methodSet = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(SET);
          }).findFirst()
          .orElse(null);

      if (Objects.nonNull(methodGet) && Objects.nonNull(methodSet)) {
        historiesProtocolMethods.put(ProtocolType.valueStringOf(field), Pair.of(methodSet, methodGet));
      }
    }

  }

  private void setParamHistoryMethods() {
    paramHistoryMethods = new EnumMap<>(ProtocolType.class);
    Method[] methods = ParamHistory.class.getDeclaredMethods();

    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (String field : fieldNames) {

      Method methodGet = Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          }).findFirst()
          .orElse(null);

      if (Objects.nonNull(methodGet)) {
        paramHistoryMethods.put(ProtocolType.valueStringOf(field), methodGet);
      }
    }
  }

  @PostConstruct
  public void setup() {
    setProtocolMethodMap();
    setMapEpochParamMethods();
    setParamHistoryMethods();
    setHistoriesProtocolMethods();
  }
}
