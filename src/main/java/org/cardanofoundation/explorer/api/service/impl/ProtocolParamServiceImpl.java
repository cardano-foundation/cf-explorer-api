package org.cardanofoundation.explorer.api.service.impl;

import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.MAINNET_NETWORK;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.PREPROD_NETWORK;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.PREVIEW_NETWORK;
import static org.cardanofoundation.explorer.api.common.constant.CommonConstant.isWithinRange;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolStatus;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.model.response.protocol.EpochChange;
import org.cardanofoundation.explorer.api.model.response.protocol.FixedProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;
import org.cardanofoundation.explorer.api.projection.LatestParamHistory;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.api.repository.CostModelRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.GenesisFetching;
import org.cardanofoundation.explorer.api.service.ProtocolParamService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.CostModel;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam_;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolParamServiceImpl implements ProtocolParamService {

  public static final String SET = "set";
  public static final String PROPOSAL = "Proposal";
  public static final String PROTOCOL_HISTORY = "PROTOCOL_HISTORY_ALL";
  public static final String ACTIVE_SLOTS_COEFF = "activeSlotsCoeff";
  public static final String GEN_DElEGS = "genDelegs";
  public static final String UPDATE_QUORUM = "updateQuorum";
  public static final String NETWORK_ID = "networkId";
  public static final String INITIAL_FUNDS = "initialFunds";
  public static final String MAX_LOVELACE_SUPPLY = "maxLovelaceSupply";
  public static final String NETWORK_MAGIC = "networkMagic";
  public static final String EPOCH_LENGTH = "epochLength";
  public static final String TIMESTAMP = "startTime";
  public static final String SLOT_SPERKES_PERIOD = "slotsPerKESPeriod";
  public static final String SLOT_LENGTH = "slotLength";
  public static final String MAX_KESEVOLUTIONS = "maxKESEvolutions";
  public static final String SECURITY_PARAM = "securityParam";

  final ParamProposalRepository paramProposalRepository;
  final EpochParamRepository epochParamRepository;
  final EpochRepository epochRepository;
  final TxRepository txRepository;
  final CostModelRepository costModelRepository;
  final ProtocolMapper protocolMapper;
  final RedisTemplate<String, HistoriesProtocol> redisTemplate;
  final GenesisFetching genesisFetching;
  final ObjectMapper objectMapper;

  @Value("${application.network}")
  String network;
  @Value("${application.epoch.days}")
  public int epochDays;
  public static final String GET = "get";
  @Value("${genesis.shelley.mainnet}")
  String shelleyMainnet;
  @Value("${genesis.shelley.preprod}")
  String shelleyPreprod;
  @Value("${genesis.shelley.preview}")
  String shelleyPreview;
  @Value("${genesis.byron.mainnet}")
  String byronMainnet;
  @Value("${genesis.byron.preprod}")
  String byronPreprod;
  @Value("${genesis.byron.preview}")
  String byronPreview;
  // key::ProtocolType, value::getter>
  Map<ProtocolType, Method> epochParamMethods;

  // key::ProtocolType, value::Pair<setter,getter>
  Map<ProtocolType, Pair<Method, Method>> protocolsMethods;

  //key::ProtocolType, value::getter
  Map<ProtocolType, Method> paramHistoryMethods;

  //key::ProtocolType, value::Pair<setter,getter>
  Map<ProtocolType, Pair<Method, Method>> historiesProtocolMethods;

  //key::ProtocolType, value::Pair<getter, getterProposal>
  Map<ProtocolType, Pair<Method, Method>> latestParamHistoryMethods;

  @Override
  public HistoriesProtocol getHistoryProtocolParameters(List<ProtocolType> protocolTypesInput,
      BigInteger startTime,
      BigInteger endTime) {
    final String redisKey = String.format("%s_%s", network, PROTOCOL_HISTORY).toUpperCase();

    boolean isGetAll = Boolean.FALSE;

    if (ObjectUtils.isEmpty(protocolTypesInput) || protocolTypesInput.contains(ProtocolType.ALL)) {
      protocolTypesInput = ProtocolType.getAll();
      isGetAll = Boolean.TRUE;
      if (Objects.nonNull(redisTemplate.opsForValue().get(redisKey)) &&
          (Objects.isNull(startTime) || Objects.isNull(endTime))) {
        return redisTemplate.opsForValue().get(redisKey);
      }
    }

    final List<ProtocolType> protocolTypes = protocolTypesInput;

    if ((Objects.isNull(startTime) && Objects.nonNull(endTime)) ||
        (Objects.isNull(endTime) && Objects.nonNull(startTime))) {
      throw new BusinessException(BusinessCode.TIME_RANGE_ILLEGAL);
    }

    Timestamp startFilterTime = null;
    Timestamp endFilterTime = null;

    if (Objects.nonNull(startTime) && Objects.nonNull(endTime)) {
      if (endTime.subtract(startTime).compareTo(BigInteger.ZERO) < BigInteger.ZERO.intValue()) {
        throw new BusinessException(BusinessCode.TIME_RANGE_ILLEGAL);
      }

      startFilterTime = Timestamp.valueOf(
          LocalDateTime.ofEpochSecond(startTime.longValue(), BigInteger.ZERO.intValue(),
              ZoneOffset.UTC));
      endFilterTime = Timestamp.valueOf(
          LocalDateTime.ofEpochSecond(endTime.longValue(), BigInteger.ZERO.intValue(),
              ZoneOffset.UTC));
    }

    // find all param proposal change, and take the last change
    List<ParamHistory> historiesChange;
    // find all epoch param group there in to map of key:epoch value:epoch_param
    Map<Integer, EpochParam> epochParams;
    if (Objects.isNull(startFilterTime) && Objects.isNull(endFilterTime)) {
      historiesChange = paramProposalRepository.findProtocolsChange();
      epochParams = epochParamRepository.findAll().stream()
          .collect(Collectors.toMap(EpochParam::getEpochNo, Function.identity(), (a, b) -> a));
    } else {
      isGetAll = Boolean.FALSE;
      historiesChange = paramProposalRepository.findProtocolsChange(endFilterTime);
      epochParams = epochParamRepository.findEpochParamInTime(endFilterTime).stream()
          .collect(Collectors.toMap(EpochParam::getEpochNo, Function.identity(), (a, b) -> a));
    }

    // find all transaction update protocol param
    Map<Long, Tx> txs = txRepository.findByIdIn(
            historiesChange.stream().map(ParamHistory::getTx).toList())
        .parallelStream().collect(Collectors.toMap(Tx::getId, Function.identity()));
    // group by epoch
    Map<Integer, List<ParamHistory>> historiesChangeByEpoch = historiesChange
        .parallelStream()
        .collect(Collectors.groupingBy(ParamHistory::getEpochNo, Collectors.toList()));

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

    final Map<Integer, EpochTimeProjection> epochTime = new HashMap<>();
    if (Objects.nonNull(startFilterTime) && Objects.nonNull(endFilterTime)) {
      epochTime.putAll(epochRepository.findEpochTime(
              min, max)
          .stream()
          .collect(Collectors
              .toMap(EpochTimeProjection::getEpochNo, Function.identity())));
    }

    AtomicReference<Protocols> currentMarkProtocols = new AtomicReference<>();

    // check unprocessedProtocols data
    IntStream.range(min, max)
        .boxed()
        .sorted(Collections.reverseOrder())
        .filter(epochParams::containsKey)
        .forEach(epoch -> {

          // fill markProtocol when it empties
          if (Objects.isNull(currentMarkProtocols.get())) {
            currentMarkProtocols.set(unprocessedProtocols.get(epoch));
          }

          Protocols markProtocol = currentMarkProtocols.get();
          Protocols currentProtocol = unprocessedProtocols.get(epoch);

          fillMissingProtocolField(currentProtocol, epochParams.get(epoch), protocolTypes);

          if (markProtocol.equals(currentProtocol, protocolsMethods, protocolTypes)) {
            currentProtocol.getEpochChange()
                .setStartEpoch(markProtocol.getEpochChange().getStartEpoch());
            currentProtocol.getEpochChange().setEndEpoch(epoch);
            currentMarkProtocols.set(currentProtocol);

            if (min.equals(epoch)) {
              fillMissingProtocolField(currentProtocol, epochParams.get(epoch), protocolTypes);
              processProtocols.add(currentProtocol);
            }
            return;
          }

          fillMissingProtocolField(markProtocol, epochParams.get(epoch), protocolTypes);

          processProtocols.add(markProtocol);
          currentMarkProtocols.set(currentProtocol);

          if (min.equals(epoch)) {
            fillMissingProtocolField(currentProtocol, epochParams.get(epoch), protocolTypes);
            processProtocols.add(currentProtocol);
          }
        });

    HistoriesProtocol historiesProtocol = protocolMapper.mapProtocolsToHistoriesProtocol(
        processProtocols, protocolsMethods, historiesProtocolMethods, protocolTypes);
    handleHistoriesChange(historiesProtocol, protocolTypes);

    if (!ObjectUtils.isEmpty(epochTime)) {
      filterProtocolTime(startFilterTime, endFilterTime, historiesProtocol, protocolTypes,
          epochTime);
    }

    if (!ObjectUtils.isEmpty(historiesProtocol.getEpochChanges())) {
      EpochChange epochChange = historiesProtocol.getEpochChanges().get(0);
      if (!epochChange.getStartEpoch().equals(epochChange.getEndEpoch())) {
        epochChange.setStartEpoch(epochChange.getEndEpoch());
      }
    }

    if (isGetAll) {
      var currentEpoch = epochRepository.findByCurrentEpochNo().get();
      var redisKeyExpireTime = currentEpoch.getStartTime().toLocalDateTime().plusDays(epochDays);
      final var seconds = ChronoUnit.SECONDS.between(LocalDateTime.now(ZoneOffset.UTC),
          redisKeyExpireTime);
      redisTemplate.opsForValue().set(redisKey, historiesProtocol);
      redisTemplate.expire(redisKey, Duration.ofSeconds(seconds));
    }

    return historiesProtocol;
  }

  @Override
  public Protocols getLatestChange() {

    List<LatestParamHistory> changeHistories = getParamHistories();
    final Protocols protocols = new Protocols();
    Map<Long, String> costModelMap = costModelRepository.findAll()
        .stream()
        .collect(Collectors.toMap(CostModel::getId, CostModel::getCosts));

    changeHistories
        .stream()
        .sorted(Comparator.comparing(LatestParamHistory::getEpochNo))
        .forEach(changeHistory ->
            ProtocolType.getAll()
                .parallelStream()
                .forEach(protocolType -> {
                  try {
                    final var protocolSetter = protocolsMethods.get(protocolType).getFirst();
                    final var protocolGetter = protocolsMethods.get(protocolType).getSecond();

                    var changeValue = latestParamHistoryMethods.get(protocolType).getFirst()
                        .invoke(changeHistory);
                    final var oldValue = (ProtocolHistory) protocolGetter.invoke(protocols);

                    final var changeTime =
                        ((boolean) latestParamHistoryMethods.get(protocolType).getSecond()
                            .invoke(changeHistory)) ? changeHistory.getBlockTime()
                            : changeHistory.getEpochTime();
                    final var transactionHash =
                        ((boolean) latestParamHistoryMethods.get(protocolType).getSecond()
                            .invoke(changeHistory)) ? changeHistory.getHash()
                            : null;

                    if (protocolType.equals(ProtocolType.COST_MODEL)) {
                      changeValue = costModelMap.get(changeValue);
                    }

                    if (Objects.isNull(oldValue)) {
                      ProtocolHistory protocolHistory = ProtocolHistory.builder()
                          .transactionHash(transactionHash)
                          .time(changeTime)
                          .epochNo(changeHistory.getEpochNo())
                          .value(changeValue)
                          .build();
                      protocolSetter.invoke(protocols, protocolHistory);
                      return;
                    }

                    if (!String.valueOf(oldValue.getValue()).equals(String.valueOf(changeValue))) {
                      oldValue.setTime(changeTime);
                      oldValue.setTransactionHash(transactionHash);
                      oldValue.setValue(changeValue);
                      oldValue.setEpochNo(changeHistory.getEpochNo());
                    }

                  } catch (IllegalAccessException | InvocationTargetException e) {
                    log.error(e.getMessage());
                  }
                })
        );
    return protocols;
  }

  @Override
  public FixedProtocol getFixedProtocols() {
    return getFixedProtocolByNetwork(network);
  }

  /**
   * histories changes of protocols in network
   *
   * @return
   */
  private List<LatestParamHistory> getParamHistories() {
    var maxEpochChange = paramProposalRepository.findMaxEpochChange();

    return paramProposalRepository.findProtocolsChange(
        maxEpochChange);
  }

  /**
   * Mapping ProtocolHistory that change
   *
   * @param currentProtocol value of protocol param
   * @param tx              transaction proposal  change
   * @param timeChange      time proposal change
   * @return
   */
  public static ProtocolHistory getChangeProtocol(Object currentProtocol, Tx tx,
      AtomicReference<LocalDateTime> timeChange) {

    timeChange.set(LocalDateTime.ofInstant(tx.getBlock().getTime().toInstant(), ZoneOffset.UTC));
    return ProtocolHistory.builder()
        .value(currentProtocol)
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .build();
  }

  /**
   * Mapping not change protocol parameters
   *
   * @param object
   * @return
   */
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

  /**
   * @param epochParam epoch param
   * @return
   */
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

  /**
   * Map epoch change
   *
   * @param epochNo
   * @return
   */
  private Protocols getEpochProtocol(Integer epochNo) {
    return Protocols.builder()
        .epochChange(EpochChange.builder()
            .startEpoch(epochNo)
            .endEpoch(epochNo)
            .build())
        .build();
  }

  /**
   * @param paramHistories proposal change
   * @param txs            transaction change
   * @param protocols      protocol
   * @param protocolTypes  list protocol type must filter
   */
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
          AtomicReference<LocalDateTime> timeChange = new AtomicReference<>(null);
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

  /**
   * Mapping ProtocolHistory that cost model propose to change
   *
   * @param costModelId cost model id
   * @param tx          transaction proposal change
   * @param timeChange  time proposal change
   * @return
   */
  private ProtocolHistory getChangeCostModelProtocol(Long costModelId,
      Tx tx,
      AtomicReference<LocalDateTime> timeChange) {

    Optional<CostModel> costModel = costModelRepository.findById(costModelId);
    timeChange.set(LocalDateTime.ofInstant(tx.getBlock().getTime().toInstant(), ZoneOffset.UTC));
    return costModel.map(model -> ProtocolHistory.builder()
        .value(model.getCosts())
        .time(tx.getBlock().getTime())
        .transactionHash(tx.getHash())
        .costModelId(costModelId)
        .build()).orElse(null);
  }

  /**
   * Fill missing protocol parameters that not change in epoch with filter condition
   *
   * @param protocols     protocol
   * @param epochParam    epoch param
   * @param protocolTypes protocol type must filter
   */
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

  /**
   * This function is a loop of prototype
   *
   * @param historiesProtocol
   * @param protocolTypes
   */
  private void handleHistoriesChange(HistoriesProtocol historiesProtocol,
      List<ProtocolType> protocolTypes) {
    historiesProtocolMethods.entrySet()
        .stream().filter(entry -> protocolTypes.contains(entry.getKey()))
        .parallel()
        .forEach(
            method -> {
              try {
                var historyProtocolGet = method.getValue().getSecond();
                log.debug("method {}", method.getValue().getSecond().getName());
                handleHistoryStatus(
                    (List<ProtocolHistory>) historyProtocolGet.invoke(historiesProtocol));
              } catch (Exception e) {
                log.error(e.getMessage());
                log.error(e.getLocalizedMessage());
              }
            }
        );
  }

  /**
   * This function fill {@link ProtocolStatus } base on change of input list
   * {@link ProtocolHistory}
   *
   * @param protocolHistories
   */
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
              .hashCode()) {
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

  /**
   * User java refection to mapping {@link EpochParam} Get method
   */
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

  /**
   * User java refection to mapping {@link Protocols} Pair of <Set,Get> method
   */
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

  /**
   * User java refection to mapping {@link HistoriesProtocol} Pair of <Set,Get> method
   */
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
        historiesProtocolMethods.put(ProtocolType.valueStringOf(field),
            Pair.of(methodSet, methodGet));
      }
    }

  }

  /**
   * User java refection to mapping {@link ParamHistory} get method
   */
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

  /**
   * Remove index out of range compare to filter time in {@link HistoriesProtocol}. Change Protocol
   * status if necessary.
   *
   * @param startFilterTime   start time to filter
   * @param endFilterTime     end time to filter
   * @param historiesProtocol list of merged epoch
   * @param protocolTypes     list of selected protocol type
   * @param epochTime         map epoch time key::epoch no,value::EpochTimeProjection
   */
  private void filterProtocolTime(Timestamp startFilterTime, Timestamp endFilterTime,
      HistoriesProtocol historiesProtocol,
      List<ProtocolType> protocolTypes,
      Map<Integer, EpochTimeProjection> epochTime) {

    List<Entry<ProtocolType, Pair<Method, Method>>> methods = historiesProtocolMethods
        .entrySet()
        .stream()
        .filter(entry -> protocolTypes.contains(entry.getKey()))
        .toList();

    List<EpochChange> epochChanges = historiesProtocol.getEpochChanges()
        .stream().map(EpochChange::clone).toList();

    // filter epoch out of range from startTime and endTime
    List<Integer> removeIndex = getOutRangeIndex(epochChanges,
        startFilterTime,
        endFilterTime, epochTime);

    // change protocol status from ADDED to NOT_CHANGE
    for (int index = BigInteger.ZERO.intValue(); index < epochChanges.size(); index = index + 1) {
      var historiesEpochChange = historiesProtocol.getEpochChanges().get(index);
      var epochChange = epochChanges.get(index);
      if (epochChange.equals(historiesEpochChange)) {
        continue;
      }

      final var checkedIndex = index;
      methods
          .parallelStream()
          .forEach(entry -> {
            var historyProtocolsGet = entry.getValue().getSecond();
            try {
              ProtocolHistory protocolHistory = ((List<ProtocolHistory>) historyProtocolsGet.invoke(
                  historiesProtocol)).get(checkedIndex);
              if (protocolHistory.getStatus().equals(ProtocolStatus.ADDED) &&
                  !historiesEpochChange.getEndEpoch().equals(epochChange.getEndEpoch())) {
                protocolHistory.setStatus(ProtocolStatus.NOT_CHANGE);
              }
            } catch (Exception e) {
              log.error(e.getMessage());
              log.error(e.getLocalizedMessage());
            }
          });
    }

    var removeIndexInOrder = removeIndex.stream()
        .sorted(Collections.reverseOrder()).toList();

    historiesProtocol.setEpochChanges(new ArrayList<>(epochChanges));
    removeIndexInOrder
        .forEach(index -> {
          historiesProtocol.getEpochChanges().remove(index.intValue());
          methods
              .parallelStream()
              .forEach(entry -> {
                var historyProtocolsGet = entry.getValue().getSecond();
                try {
                  ((List<ProtocolHistory>) historyProtocolsGet.invoke(historiesProtocol)).remove(
                      index.intValue());
                } catch (Exception e) {
                  log.error(e.getMessage());
                  log.error(e.getLocalizedMessage());
                }
              });
        });
  }

  /**
   * this function compare merged epoch start time and end time to decide which epoch would be
   * removed from epoch change list
   *
   * @param list        list of merged epoch
   * @param startFilter time start filter
   * @param endFilter   time end filter
   * @param epochTime   map start and end time of epoch
   * @return
   */
  List<Integer> getOutRangeIndex(List<EpochChange> list,
      Timestamp startFilter,
      Timestamp endFilter,
      Map<Integer, EpochTimeProjection> epochTime) {

    return IntStream.range(BigInteger.ZERO.intValue(), list.size())
        .boxed()
        .filter(index -> {
          EpochChange epochChange = list.get(index);
          var epochChangeStartTime = epochTime.get(epochChange.getEndEpoch()).getStartTime();
          var epochChangeEndTime = epochTime.get(epochChange.getStartEpoch()).getEndTime();
          // check if filter time range of epoch time or not
          var inRange = isWithinRange(startFilter, epochChangeStartTime, epochChangeEndTime)
              || isWithinRange(endFilter, epochChangeStartTime, epochChangeEndTime);
          // check if epoch time in filter range
          if (!inRange) {
            inRange = isWithinRange(epochChangeStartTime, startFilter, endFilter)
                || isWithinRange(epochChangeEndTime, startFilter, endFilter);
          }

          List<Integer> inRangeEpoch = new ArrayList<>();
          if (inRange && !epochChange.getEndEpoch().equals(epochChange.getStartEpoch())) {
            IntStream.range(epochChange.getEndEpoch(),
                    epochChange.getStartEpoch() + BigInteger.ONE.intValue())
                .boxed()
                .sorted(Collections.reverseOrder())
                .forEach(epoch -> {
                  final var epochStartTime = epochTime.get(epoch).getStartTime();
                  final var epochEndTime = epochTime.get(epoch).getEndTime();

                  Boolean inEpochRange = isWithinRange(startFilter, epochStartTime, epochEndTime)
                      || isWithinRange(endFilter, epochStartTime, epochEndTime);

                  if (Boolean.FALSE.equals(inEpochRange)) {
                    inEpochRange = isWithinRange(epochStartTime, startFilter, endFilter)
                        || isWithinRange(epochEndTime, startFilter, endFilter);
                  }

                  if (Boolean.FALSE.equals(inEpochRange)) {
                    return;
                  }
                  inRangeEpoch.add(epoch);
                });
            epochChange.setStartEpoch(inRangeEpoch.get(BigInteger.ZERO.intValue()));
            epochChange.setEndEpoch(
                inRangeEpoch.get(inRangeEpoch.size() - BigInteger.ONE.intValue()));
          }

          return !inRange;
        }).toList();
  }

  /**
   * User java refection to mapping {@link ParamHistory} get method
   */
  private void setLatestParamHistoryMethods() {
    latestParamHistoryMethods = new EnumMap<>(ProtocolType.class);
    Method[] methods = LatestParamHistory.class.getDeclaredMethods();

    List<String> fieldNames = Arrays.stream(ProtocolType.values())
        .map(ProtocolType::getFieldName).toList();

    for (String field : fieldNames) {

      AtomicReference<Method> getter = new AtomicReference<>();
      AtomicReference<Method> getterProposal = new AtomicReference<>();

      Arrays.stream(methods)
          .filter(method -> {
            var methodLowerCase = method.getName().toLowerCase();
            var fieldLowerCase = field.toLowerCase();
            return methodLowerCase.contains(fieldLowerCase) && methodLowerCase.contains(GET);
          })
          .forEach(method -> {
            if (method.getName().contains(PROPOSAL)) {
              getterProposal.set(method);
              return;
            }
            getter.set(method);
          });

      if (Objects.isNull(getter.get())) {
        return;
      }
      Pair<Method, Method> methodPair = Pair.of(getter.get(), getterProposal.get());
      latestParamHistoryMethods.put(ProtocolType.valueStringOf(field), methodPair);
    }
  }

  private FixedProtocol getFixedProtocolByNetwork(String network) {
    FixedProtocol fixedProtocol;
    switch (network) {
      case MAINNET_NETWORK -> {
        fixedProtocol = getFixedProtocolFromShelleyGenesis(shelleyMainnet);
        return getFixedProtocolFromByronGenesis(byronMainnet, fixedProtocol);
      }
      case PREPROD_NETWORK -> {
        fixedProtocol = getFixedProtocolFromShelleyGenesis(shelleyPreprod);
        return getFixedProtocolFromByronGenesis(byronPreprod, fixedProtocol);
      }
      case PREVIEW_NETWORK -> {
        fixedProtocol = getFixedProtocolFromShelleyGenesis(shelleyPreview);
        return getFixedProtocolFromByronGenesis(byronPreview, fixedProtocol);
      }
      default -> {
        return null;
      }
    }
  }

  private FixedProtocol getFixedProtocolFromShelleyGenesis(String genesisShelley) {

    log.info("Read protocol data from url {}", genesisShelley);
    String genesisShelleyJson = genesisFetching.getContent(genesisShelley);
    try {
      Map<String, Object> genesisShelleyJsonMap = objectMapper.readValue(genesisShelleyJson,
          new TypeReference<>() {
          });
      return FixedProtocol.builder()
          .activeSlotsCoeff((Double) genesisShelleyJsonMap.get(ACTIVE_SLOTS_COEFF))
          .genDelegs(genesisShelleyJsonMap.get(GEN_DElEGS))
          .updateQuorum((Integer) genesisShelleyJsonMap.get(UPDATE_QUORUM))
          .networkId((String) genesisShelleyJsonMap.get(NETWORK_ID))
          .initialFunds(genesisShelleyJsonMap.get(INITIAL_FUNDS))
          .maxLovelaceSupply(
              convertObjecToBigInteger(genesisShelleyJsonMap.get(MAX_LOVELACE_SUPPLY)))
          .networkMagic((Integer) genesisShelleyJsonMap.get(NETWORK_MAGIC))
          .epochLength((Integer) genesisShelleyJsonMap.get(EPOCH_LENGTH))
          .slotsPerKESPeriod((Integer) genesisShelleyJsonMap.get(SLOT_SPERKES_PERIOD))
          .slotLength((Integer) genesisShelleyJsonMap.get(SLOT_LENGTH))
          .maxKESEvolutions((Integer) genesisShelleyJsonMap.get(MAX_KESEVOLUTIONS))
          .securityParam((Integer) genesisShelleyJsonMap.get(SECURITY_PARAM))
          .build();
    } catch (Exception e) {
      log.error("Genesis data at {} can't parse from json to java object", genesisShelley);
      log.error("{} value \n {}", genesisShelley, genesisShelleyJson);
      log.error("{}", e.getMessage());
      System.exit(0);
    }
    return null;
  }

  private FixedProtocol getFixedProtocolFromByronGenesis(String genesisByron,
      FixedProtocol fixedProtocol) {

    log.info("Read protocol data from url {}", genesisByron);
    String genesisByronJson = genesisFetching.getContent(genesisByron);
    try {
      Map<String, Object> genesisByronJsonMap = objectMapper.readValue(genesisByronJson,
          new TypeReference<>() {
          });
      Integer startTime = (Integer) genesisByronJsonMap.get(TIMESTAMP);
      SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
      dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
      fixedProtocol.setTimestamp(
          dateFormat.format(new Date(new Timestamp(startTime * 1000L).getTime())));
      return fixedProtocol;
    } catch (Exception e) {
      log.error("Genesis data at {} can't parse from json to java object", genesisByron);
      log.error("{} value \n {}", genesisByron, genesisByronJson);
      log.error("{}", e.getMessage());
      System.exit(0);
    }
    return null;
  }

  private BigInteger convertObjecToBigInteger(Object o) {
    return new BigInteger(String.valueOf(o));
  }

  @PostConstruct
  public void setup() {
    setProtocolMethodMap();
    setMapEpochParamMethods();
    setParamHistoryMethods();
    setHistoriesProtocolMethods();
    setLatestParamHistoryMethods();
  }
}
