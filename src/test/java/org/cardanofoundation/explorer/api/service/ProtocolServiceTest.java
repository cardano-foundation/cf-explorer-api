package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.springframework.test.context.ContextConfiguration;

import com.google.common.base.Objects;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolStatus;
import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.model.response.protocol.EpochChange;
import org.cardanofoundation.explorer.api.model.response.protocol.HistoriesProtocol;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.projection.EpochTimeProjection;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.api.repository.CostModelRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.EpochTimeProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.ParamHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.CostModel;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.mapstruct.factory.Mappers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl.getChangeProtocol;
import static org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl.mapProtocols;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProtocolMapper.class)
class ProtocolServiceTest {

  @Mock
  ParamProposalRepository paramProposalRepository;
  @Mock
  EpochParamRepository epochParamRepository;
  @Mock
  TxRepository txRepository;
  @Mock
  CostModelRepository costModelRepository;
  @Mock
  EpochRepository epochRepository;
  @Spy
  ProtocolMapper protocolMapper = Mappers.getMapper(ProtocolMapper.class);

  @InjectMocks
  private ProtocolParamServiceImpl protocolParamService;

  @BeforeEach
  void setup() {
    protocolParamService.setup();
  }

  @BeforeEach
  void before() {
  }

  final static long baseTime = 1685491200L;


  private Timestamp getTimeStamp(long days) {
    return Timestamp.valueOf(LocalDateTime.now().minusDays(days));
  }

  private Timestamp getTimeStampUTC(long days) {
    var time = LocalDateTime.ofEpochSecond(days, 0, ZoneOffset.UTC);
    return Timestamp.valueOf(time);
  }

  private Timestamp getTimeStampPlusHours(long days, long hours) {
    var time = LocalDateTime.ofEpochSecond(days, 0, ZoneOffset.UTC);
    return Timestamp.valueOf(time.plusHours(hours));
  }

  private Timestamp getTimeStampPlusDays(long days, long day, long seconds) {
    var time = LocalDateTime.ofEpochSecond(days + seconds, 0, ZoneOffset.UTC);
    return Timestamp.valueOf(time.plusDays(day));
  }

  private EpochParam getBuildEpochParam(BigInteger zero) {
    return EpochParam.builder()
        .id(zero.longValue())
        .epochNo(zero.intValue())
        .build();
  }

  private ParamHistoryProjection getBuildParamHistory(BigInteger zero) {
    return ParamHistoryProjection.builder()
        .id(zero.longValue())
        .tx(zero.longValue())
        .epochNo(zero.intValue())
        .build();
  }

  // Histories Change with date filter
  private void setupData(List<ParamHistory> protocolHistories,
                         List<EpochParam> epochParams,
                         List<Tx> transaction,
                         List<CostModel> costModels,
                         List<EpochTimeProjection> epoch) {

    EpochTimeProjection epoch1 = EpochTimeProjectionImpl.builder()
        .epochNo(1)
        .startTime(getTimeStampUTC(baseTime))
        .endTime(getTimeStampPlusDays(baseTime, 5, 0))
        .build();

    EpochTimeProjection epoch2 = EpochTimeProjectionImpl.builder()
        .epochNo(2)
        .startTime(getTimeStampPlusDays(baseTime, 5, 1))
        .endTime(getTimeStampPlusDays(baseTime, 10, 0))
        .build();

    EpochTimeProjection epoch3 = EpochTimeProjectionImpl.builder()
        .epochNo(3)
        .startTime(getTimeStampPlusDays(baseTime, 10, 1))
        .endTime(getTimeStampPlusDays(baseTime, 15, 0))
        .build();

    EpochTimeProjection epoch4 = EpochTimeProjectionImpl.builder()
        .epochNo(4)
        .startTime(getTimeStampPlusDays(baseTime, 15, 1))
        .endTime(getTimeStampPlusDays(baseTime, 20, 0))
        .build();

    EpochTimeProjection epoch5 = EpochTimeProjectionImpl.builder()
        .epochNo(5)
        .startTime(getTimeStampPlusDays(baseTime, 20, 1))
        .endTime(getTimeStampPlusDays(baseTime, 25, 0))
        .build();

    EpochTimeProjection epoch6 = EpochTimeProjectionImpl.builder()
        .epochNo(6)
        .startTime(getTimeStampPlusDays(baseTime, 25, 1))
        .endTime(getTimeStampPlusDays(baseTime, 30, 0))
        .build();

    epoch.add(epoch1);
    epoch.add(epoch2);
    epoch.add(epoch3);
    epoch.add(epoch4);
    epoch.add(epoch5);
    epoch.add(epoch6);

    Tx firstTransaction = Tx.builder()
        .id(1L)
        .block(Block.builder().time(getTimeStampPlusHours(baseTime, 1)).build())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
            .time(getTimeStampPlusDays(baseTime, 5, 10))
            .build())
        .build();

    Tx secondTransaction = Tx.builder()
        .id(2L)
        .block(Block.builder()
            .time(getTimeStampPlusHours(getTimeStampPlusDays(baseTime, 5, 0).getSeconds(), 1))
            .build())
        .hash(BigInteger.TWO.toString())
        .block(Block.builder()
            .time(getTimeStampPlusDays(baseTime, 20, 8))
            .build())
        .build();

    transaction.add(firstTransaction);
    transaction.add(secondTransaction);

    CostModel genesisCostModel = CostModel.builder()
        .costs("0")
        .build();

    CostModel costModelOne = CostModel.builder()
        .costs("1")
        .build();

    CostModel costModelTwo = CostModel.builder()
        .costs("2")
        .build();

    costModels.add(costModelOne);
    costModels.add(costModelTwo);

    // this will change epoch param at epoch 3
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.ONE).tx(BigInteger.ONE.longValue()).minFeeB(BigInteger.ONE)
        .maxBlockSize(BigInteger.ONE).maxTxSize(BigInteger.ONE).maxBhSize(BigInteger.ONE)
        .keyDeposit(BigInteger.ONE).poolDeposit(BigInteger.ONE).maxEpoch(BigInteger.ONE)
        .optimalPoolCount(BigInteger.ONE).maxTxExMem(BigInteger.ONE)
        .maxTxExSteps(BigInteger.ONE).influence(BigInteger.ONE.doubleValue())
        .maxBlockExMem(BigInteger.ONE).maxBlockExSteps(BigInteger.ONE)
        .maxValSize(BigInteger.ONE).coinsPerUtxoSize(BigInteger.ONE)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .decentralisation(BigInteger.ONE.doubleValue()).priceMem(BigInteger.ONE.doubleValue())
        .priceStep(BigInteger.ONE.doubleValue()).protocolMajor(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue()).collateralPercent(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue()).extraEntropy(BigInteger.ONE.toString())
        .costModel(BigInteger.ONE.longValue())
        .time(firstTransaction.getBlock().getTime())
        .minUtxoValue(BigInteger.ONE).minPoolCost(BigInteger.ONE).epochNo(2)
        .build();
    // this will change epoch param at epoch 6
    ParamHistoryProjection protocolChangeEpochFive = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.TWO).tx(BigInteger.TWO.longValue()).minFeeB(BigInteger.TWO)
        .maxBlockSize(BigInteger.TWO).maxTxSize(BigInteger.TWO).maxBhSize(BigInteger.TWO)
        .keyDeposit(BigInteger.TWO).poolDeposit(BigInteger.TWO).maxEpoch(BigInteger.TWO)
        .optimalPoolCount(BigInteger.TWO).maxTxExMem(BigInteger.TWO)
        .maxTxExSteps(BigInteger.TWO).influence(BigInteger.TWO.doubleValue())
        .maxBlockExMem(BigInteger.TWO).maxBlockExSteps(BigInteger.TWO)
        .maxValSize(BigInteger.TWO).coinsPerUtxoSize(BigInteger.TWO)
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .decentralisation(BigInteger.TWO.doubleValue()).priceMem(BigInteger.TWO.doubleValue())
        .priceStep(BigInteger.TWO.doubleValue()).protocolMajor(BigInteger.TWO.intValue())
        .protocolMinor(BigInteger.TWO.intValue()).collateralPercent(BigInteger.TWO.intValue())
        .maxCollateralInputs(BigInteger.TWO.intValue()).extraEntropy(BigInteger.TWO.toString())
        .costModel(BigInteger.TWO.longValue())
        .time(secondTransaction.getBlock().getTime())
        .minUtxoValue(BigInteger.TWO).minPoolCost(BigInteger.TWO).epochNo(5)
        .build();

    protocolHistories.add(protocolChangeEpochTwo);
    protocolHistories.add(protocolChangeEpochFive);

    EpochParam genesis = EpochParam.builder()
        .minFeeA(BigInteger.ZERO.intValue()).minFeeB(BigInteger.ZERO.intValue())
        .maxBlockSize(BigInteger.ZERO.intValue()).maxTxSize(BigInteger.ZERO.intValue())
        .maxBhSize(BigInteger.ZERO.intValue()).keyDeposit(BigInteger.ZERO)
        .poolDeposit(BigInteger.ZERO).maxEpoch(BigInteger.ZERO.intValue())
        .optimalPoolCount(BigInteger.ZERO.intValue()).maxTxExMem(BigInteger.ZERO)
        .maxTxExSteps(BigInteger.ZERO).influence(BigInteger.ZERO.doubleValue())
        .maxBlockExMem(BigInteger.ZERO).maxBlockExSteps(BigInteger.ZERO)
        .maxValSize(BigInteger.ZERO).coinsPerUtxoSize(BigInteger.ZERO)
        .monetaryExpandRate(BigInteger.ZERO.doubleValue())
        .treasuryGrowthRate(BigInteger.ZERO.doubleValue())
        .decentralisation(BigInteger.ZERO.doubleValue()).priceMem(BigInteger.ZERO.doubleValue())
        .priceStep(BigInteger.ZERO.doubleValue()).protocolMajor(BigInteger.ZERO.intValue())
        .protocolMinor(BigInteger.ZERO.intValue()).collateralPercent(BigInteger.ZERO.intValue())
        .maxCollateralInputs(BigInteger.ZERO.intValue()).extraEntropy(BigInteger.ZERO.toString())
        .costModel(genesisCostModel)
        .minUtxoValue(BigInteger.ZERO).minPoolCost(BigInteger.ZERO).epochNo(1)
        .build();

    EpochParam epochParam1 = EpochParam.builder()
        .minFeeA(BigInteger.ZERO.intValue()).minFeeB(BigInteger.ZERO.intValue())
        .maxBlockSize(BigInteger.ZERO.intValue()).maxTxSize(BigInteger.ZERO.intValue())
        .maxBhSize(BigInteger.ZERO.intValue()).keyDeposit(BigInteger.ZERO)
        .poolDeposit(BigInteger.ZERO).maxEpoch(BigInteger.ZERO.intValue())
        .optimalPoolCount(BigInteger.ZERO.intValue()).maxTxExMem(BigInteger.ZERO)
        .maxTxExSteps(BigInteger.ZERO).influence(BigInteger.ZERO.doubleValue())
        .maxBlockExMem(BigInteger.ZERO).maxBlockExSteps(BigInteger.ZERO)
        .maxValSize(BigInteger.ZERO).coinsPerUtxoSize(BigInteger.ZERO)
        .monetaryExpandRate(BigInteger.ZERO.doubleValue())
        .treasuryGrowthRate(BigInteger.ZERO.doubleValue())
        .decentralisation(BigInteger.ZERO.doubleValue()).priceMem(BigInteger.ZERO.doubleValue())
        .priceStep(BigInteger.ZERO.doubleValue()).protocolMajor(BigInteger.ZERO.intValue())
        .protocolMinor(BigInteger.ZERO.intValue()).collateralPercent(BigInteger.ZERO.intValue())
        .maxCollateralInputs(BigInteger.ZERO.intValue()).extraEntropy(BigInteger.ZERO.toString())
        .costModel(genesisCostModel)
        .minUtxoValue(BigInteger.ZERO).minPoolCost(BigInteger.ZERO).epochNo(2)
        .build();
    // change at epoch param
    EpochParam epochParam2 = EpochParam.builder()
        .minFeeA(BigInteger.ONE.intValue()).minFeeB(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue()).maxTxSize(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue()).keyDeposit(BigInteger.ONE)
        .poolDeposit(BigInteger.ONE).maxEpoch(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue()).maxTxExMem(BigInteger.ONE)
        .maxTxExSteps(BigInteger.ONE).influence(BigInteger.ONE.doubleValue())
        .maxBlockExMem(BigInteger.ONE).maxBlockExSteps(BigInteger.ONE)
        .maxValSize(BigInteger.ONE).coinsPerUtxoSize(BigInteger.ONE)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .decentralisation(BigInteger.ONE.doubleValue()).priceMem(BigInteger.ONE.doubleValue())
        .priceStep(BigInteger.ONE.doubleValue()).protocolMajor(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue()).collateralPercent(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue()).extraEntropy(BigInteger.ONE.toString())
        .costModel(costModelOne)
        .minUtxoValue(BigInteger.ONE).minPoolCost(BigInteger.ONE).epochNo(3)
        .build();

    EpochParam epochParam3 = EpochParam.builder()
        .minFeeA(BigInteger.ONE.intValue()).minFeeB(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue()).maxTxSize(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue()).keyDeposit(BigInteger.ONE)
        .poolDeposit(BigInteger.ONE).maxEpoch(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue()).maxTxExMem(BigInteger.ONE)
        .maxTxExSteps(BigInteger.ONE).influence(BigInteger.ONE.doubleValue())
        .maxBlockExMem(BigInteger.ONE).maxBlockExSteps(BigInteger.ONE)
        .maxValSize(BigInteger.ONE).coinsPerUtxoSize(BigInteger.ONE)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .decentralisation(BigInteger.ONE.doubleValue()).priceMem(BigInteger.ONE.doubleValue())
        .priceStep(BigInteger.ONE.doubleValue()).protocolMajor(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue()).collateralPercent(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue()).extraEntropy(BigInteger.ONE.toString())
        .costModel(costModelOne)
        .minUtxoValue(BigInteger.ONE).minPoolCost(BigInteger.ONE).epochNo(4)
        .build();

    EpochParam epochParam4 = EpochParam.builder()
        .minFeeA(BigInteger.ONE.intValue()).minFeeB(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue()).maxTxSize(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue()).keyDeposit(BigInteger.ONE)
        .poolDeposit(BigInteger.ONE).maxEpoch(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue()).maxTxExMem(BigInteger.ONE)
        .maxTxExSteps(BigInteger.ONE).influence(BigInteger.ONE.doubleValue())
        .maxBlockExMem(BigInteger.ONE).maxBlockExSteps(BigInteger.ONE)
        .maxValSize(BigInteger.ONE).coinsPerUtxoSize(BigInteger.ONE)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .decentralisation(BigInteger.ONE.doubleValue()).priceMem(BigInteger.ONE.doubleValue())
        .priceStep(BigInteger.ONE.doubleValue()).protocolMajor(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue()).collateralPercent(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue()).extraEntropy(BigInteger.ONE.toString())
        .costModel(costModelOne)
        .minUtxoValue(BigInteger.ONE).minPoolCost(BigInteger.ONE).epochNo(5)
        .build();

    EpochParam epochParam5 = EpochParam.builder()
        .minFeeA(BigInteger.TWO.intValue()).minFeeB(BigInteger.TWO.intValue())
        .maxBlockSize(BigInteger.TWO.intValue()).maxTxSize(BigInteger.TWO.intValue())
        .maxBhSize(BigInteger.TWO.intValue()).keyDeposit(BigInteger.TWO)
        .poolDeposit(BigInteger.TWO).maxEpoch(BigInteger.TWO.intValue())
        .optimalPoolCount(BigInteger.TWO.intValue()).maxTxExMem(BigInteger.TWO)
        .maxTxExSteps(BigInteger.TWO).influence(BigInteger.TWO.doubleValue())
        .maxBlockExMem(BigInteger.TWO).maxBlockExSteps(BigInteger.TWO)
        .maxValSize(BigInteger.TWO).coinsPerUtxoSize(BigInteger.TWO)
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .decentralisation(BigInteger.TWO.doubleValue()).priceMem(BigInteger.TWO.doubleValue())
        .priceStep(BigInteger.TWO.doubleValue()).protocolMajor(BigInteger.TWO.intValue())
        .protocolMinor(BigInteger.TWO.intValue()).collateralPercent(BigInteger.TWO.intValue())
        .maxCollateralInputs(BigInteger.TWO.intValue()).extraEntropy(BigInteger.TWO.toString())
        .costModel(costModelOne)
        .minUtxoValue(BigInteger.TWO).minPoolCost(BigInteger.TWO).epochNo(6)
        .build();

    epochParams.add(genesis);// genesis
    epochParams.add(epochParam1); // 1
    epochParams.add(epochParam2); //2
    epochParams.add(epochParam3); //3
    epochParams.add(epochParam4); //4
    epochParams.add(epochParam5); //5
  }

  @Test
  void testChangeHistoryChangeEpoch1() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.plusDays(0).toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(2).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);
    epochParams.remove(4);
    epochs.remove(4);
    epochParams.remove(3);
    epochs.remove(3);
    epochParams.remove(2);
    epochs.remove(2);
    epochParams.remove(1);
    epochs.remove(1);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> expectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(1)
            .endEpoch(1)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(1, actual.getMinFeeA().size());
    Assertions.assertEquals(expectEpochChanges, actual.getEpochChanges());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .status(ProtocolStatus.ADDED)
        .build().hashCode(), histories.get(0).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch2() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.plusDays(6).toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(14).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);
    epochParams.remove(4);
    epochs.remove(4);
    epochParams.remove(3);
    epochs.remove(3);
    epochParams.remove(2);
    epochs.remove(2);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> expectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(2)
            .endEpoch(2)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(1, actual.getMinFeeA().size());
    Assertions.assertEquals(expectEpochChanges, actual.getEpochChanges());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .status(ProtocolStatus.NOT_CHANGE)
        .build().hashCode(), histories.get(0).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch3() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime =BigInteger.valueOf( markTime.plusDays(11).toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(14).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);
    epochParams.remove(4);
    epochs.remove(4);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> excpectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(3)
            .endEpoch(3)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(1, actual.getMinFeeA().size());
    Assertions.assertEquals(excpectEpochChanges, actual.getEpochChanges());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ONE)
        .transactionHash("1")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build().hashCode(), histories.get(0).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch2To4() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.plusDays(6).toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(19).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);
    epochParams.remove(4);
    epochs.remove(4);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> excpectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(3)
            .endEpoch(3)
            .build(),
        EpochChange.builder()
            .startEpoch(2)
            .endEpoch(2)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(2, actual.getMinFeeA().size());
    Assertions.assertEquals(excpectEpochChanges, actual.getEpochChanges());


    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ONE)
        .transactionHash("1")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build().hashCode(), histories.get(0).hashCode());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .status(ProtocolStatus.NOT_CHANGE)
        .build().hashCode(), histories.get(1).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch0To4() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(19).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);
    epochParams.remove(4);
    epochs.remove(4);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> excpectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(3)
            .endEpoch(3)
            .build(),
        EpochChange.builder()
            .startEpoch(2)
            .endEpoch(1)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(2, actual.getMinFeeA().size());
    Assertions.assertEquals(excpectEpochChanges, actual.getEpochChanges());


    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ONE)
        .transactionHash("1")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build().hashCode(), histories.get(0).hashCode());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.ADDED)
        .build().hashCode(), histories.get(1).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch0To5() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(23).toEpochSecond(ZoneOffset.UTC));

    epochParams.remove(5);
    epochs.remove(5);

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> excpectEpochChanges = List.of(
        EpochChange.builder()
            .startEpoch(3)
            .endEpoch(3)
            .build(),
        EpochChange.builder()
            .startEpoch(2)
            .endEpoch(1)
            .build());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(2, actual.getMinFeeA().size());
    Assertions.assertEquals(excpectEpochChanges, actual.getEpochChanges());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ONE)
        .transactionHash("1")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build().hashCode(), histories.get(0).hashCode());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.ADDED)
        .build().hashCode(), histories.get(1).hashCode());
  }

  @Test
  void testChangeHistoryChangeEpoch0To6() {
    List<ParamHistory> protocolHistories = new ArrayList<>();
    List<EpochParam> epochParams = new ArrayList<>();
    List<Tx> transaction = new ArrayList<>();
    List<CostModel> costModels = new ArrayList<>();
    List<EpochTimeProjection> epochs = new ArrayList<>();
    setupData(protocolHistories,
        epochParams,
        transaction,
        costModels,
        epochs);

    var markTime = LocalDateTime.ofEpochSecond(baseTime, 0, ZoneOffset.UTC);
    var startTime = BigInteger.valueOf(markTime.toEpochSecond(ZoneOffset.UTC));
    var endTime = BigInteger.valueOf(markTime.plusDays(30).toEpochSecond(ZoneOffset.UTC));

    when(paramProposalRepository.findProtocolsChange(any(Timestamp.class)))
        .thenReturn(protocolHistories);

    when(epochParamRepository.findEpochParamInTime(any(Timestamp.class)))
        .thenReturn(epochParams);

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(transaction);

    when(epochRepository.findEpochTime(any(), any()))
        .thenReturn(epochs);

    var actual = protocolParamService.getHistoryProtocolParameters(List.of(ProtocolType.MIN_FEE_A),
        startTime,
        endTime);

    List<EpochChange> excpectEpochChanges = List.of(EpochChange.builder()
            .startEpoch(6)
            .endEpoch(6)
            .build(),
        EpochChange.builder()
            .startEpoch(5)
            .endEpoch(3)
            .build(),
        EpochChange.builder()
            .startEpoch(2)
            .endEpoch(1)
            .build());

    Assertions.assertEquals(excpectEpochChanges, actual.getEpochChanges());

    List<ProtocolHistory> histories = actual.getMinFeeA();

    Assertions.assertEquals(3, actual.getMinFeeA().size());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.TWO)
        .transactionHash("2")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build(), histories.get(0));

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ONE)
        .transactionHash("1")
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.UPDATED)
        .build().hashCode(), histories.get(1).hashCode());

    Assertions.assertEquals(ProtocolHistory
        .builder()
        .value(BigInteger.ZERO)
        .time(transaction.get(1).getBlock().getTime())
        .status(ProtocolStatus.ADDED)
        .build().hashCode(), histories.get(2).hashCode());
  }


  // Histories Change
  @Test
  void testChangedHistoriesMinFeeA() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeA(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_A), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeA(List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
            ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMinFeeA(), actual.getMinFeeA());
  }

  @Test
  void testChangedHistoriesMinFeeB() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeB(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeB(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_B), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeB(List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
            ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMinFeeB(), actual.getMinFeeB());
  }

  @Test
  void testChangedHistoriesMaxBlockSize() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockSize(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockSize(), actual.getMaxBlockSize());
  }

  @Test
  void testChangedHistoriesMaxTxSize() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxSize(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxSize(), actual.getMaxTxSize());
  }

  @Test
  void testChangedHistoriesMaxBhSize() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBhSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBhSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BH_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBhSize(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxBhSize(), actual.getMaxBhSize());
  }

  @Test
  void testChangedHistoriesKeyDeposit() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .keyDeposit(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .keyDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.KEY_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .keyDeposit(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getKeyDeposit(), actual.getKeyDeposit());
  }

  @Test
  void testChangedHistoriesPoolDeposit() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .poolDeposit(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .poolDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.POOL_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .poolDeposit(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getPoolDeposit(), actual.getPoolDeposit());
  }

  @Test
  void testChangedHistoriesMaxEpoch() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxEpoch(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxEpoch(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_EPOCH), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxEpoch(List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
            ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxEpoch(), actual.getMaxEpoch());
  }

  @Test
  void testChangedHistoriesOptimalPoolCount() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .optimalPoolCount(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .optimalPoolCount(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.OPTIMAL_POOL_COUNT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .optimalPoolCount(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getOptimalPoolCount(), actual.getOptimalPoolCount());
  }

  @Test
  void testChangedHistoriesMaxTxExMem() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExMem(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExMem(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxExMem(), actual.getMaxTxExMem());
  }

  @Test
  void testChangedHistoriesMaxTxExSteps() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExSteps(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExSteps(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxExSteps(), actual.getMaxTxExSteps());
  }

  @Test
  void testChangedHistoriesInfluence() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .influence(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .influence(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.INFLUENCE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .influence(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getInfluence(), actual.getInfluence());
  }

  @Test
  void testChangedHistoriesMaxBlockExMem() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExMem(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExMem(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockExMem(), actual.getMaxBlockExMem());
  }

  @Test
  void testChangedHistoriesMaxBlockExSteps() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExSteps(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExSteps(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockExSteps(), actual.getMaxBlockExSteps());
  }

  @Test
  void testChangedHistoriesMaxValSize() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxValSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxValSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_VAL_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxValSize(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxValSize(), actual.getMaxValSize());
  }

  @Test
  void testChangedHistoriesCoinsPerUtxoSize() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .coinsPerUtxoSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .coinsPerUtxoSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COINS_PER_UTXO_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .coinsPerUtxoSize(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getCoinsPerUtxoSize(), actual.getCoinsPerUtxoSize());
  }

  @Test
  void testChangedHistoriesMonetaryExpandRate() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MONETARY_EXPAND_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .monetaryExpandRate(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMonetaryExpandRate(), actual.getMonetaryExpandRate());
  }

  @Test
  void testChangedHistoriesTreasuryGrowthRate() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.TREASURY_GROWTH_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .treasuryGrowthRate(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getTreasuryGrowthRate(), actual.getTreasuryGrowthRate());
  }

  @Test
  void testChangedHistoriesDecentralisation() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .decentralisation(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .decentralisation(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.DECENTRALISATION), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .decentralisation(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getDecentralisation(), actual.getDecentralisation());
  }

  @Test
  void testChangedHistoriesPriceMem() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceMem(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceMem(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceMem(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getPriceMem(), actual.getPriceMem());
  }

  @Test
  void testChangedHistoriesPriceStep() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceStep(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceStep(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_STEP), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceStep(
            List.of(ProtocolHistory.builder().value(2D).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1D).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getPriceStep(), actual.getPriceStep());
  }

  @Test
  void testChangedHistoriesProtocolMajor() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMajor(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMajor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MAJOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMajor(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getProtocolMajor(), actual.getProtocolMajor());
  }

  @Test
  void testChangedHistoriesProtocolMinor() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMinor(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMinor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MINOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMinor(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getProtocolMinor(), actual.getProtocolMinor());
  }

  @Test
  void testChangedHistoriesCollateralPercent() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .collateralPercent(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .collateralPercent(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COLLATERAL_PERCENT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .collateralPercent(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getCollateralPercent(), actual.getCollateralPercent());
  }

  @Test
  void testChangedHistoriesMaxCollateralInputs() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxCollateralInputs(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxCollateralInputs(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_COLLATERAL_INPUTS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxCollateralInputs(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMaxCollateralInputs(), actual.getMaxCollateralInputs());
  }

  @Test
  void testChangedHistoriesExtraEntropy() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .extraEntropy(BigInteger.TWO.toString())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .extraEntropy(BigInteger.TWO.toString())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.ENTROPY), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .entropy(
            List.of(ProtocolHistory.builder().value("2").status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value("1").status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getEntropy(), actual.getEntropy());
  }

  @Test
  void testChangedHistoriesCostModel() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .costModel(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .costModel(CostModel.builder().id(2L).costs(BigInteger.TWO.toString()).build())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    when(costModelRepository.findById(any(Long.class)))
        .thenReturn(Optional.of(CostModel.builder()
            .id(2L)
            .costs("2")
            .build()));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COST_MODEL), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .costModel(
            List.of(ProtocolHistory.builder().value("2").status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value("1").status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getCostModel(), actual.getCostModel());
  }

  @Test
  void testChangedHistoriesMinUtxoValue() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minUtxoValue(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minUtxoValue(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_UTXO_VALUE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minUtxoValue(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMinUtxoValue(), actual.getMinUtxoValue());
  }

  @Test
  void testChangedHistoriesMinPoolCost() {

    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minPoolCost(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minPoolCost(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx tx = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(tx));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_POOL_COST), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minPoolCost(
            List.of(ProtocolHistory.builder().value(2).status(ProtocolStatus.UPDATED).build(),
                ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMinPoolCost(), actual.getMinPoolCost());
  }


  // History Added
  @Test
  void testAddedHistoriesMinFeeA() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_A), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeA(List.of(ProtocolHistory.builder().value(1).status(ProtocolStatus.ADDED).build()))
        .build();
    Assertions.assertEquals(expect.getMinFeeA(), actual.getMinFeeA());
  }

  @Test
  void testAddedHistoriesMinFeeB() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_B), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeB(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMinFeeB(), actual.getMinFeeB());
  }

  @Test
  void testAddedHistoriesMaxBlockSize() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockSize(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockSize(), actual.getMaxBlockSize());
  }

  @Test
  void testAddedHistoriesMaxTxSize() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxSize(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxSize(), actual.getMaxTxSize());
  }

  @Test
  void testAddedHistoriesMaxBhSize() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BH_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBhSize(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxBhSize(), actual.getMaxBhSize());
  }

  @Test
  void testAddedHistoriesKeyDeposit() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .keyDeposit(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.KEY_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .keyDeposit(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getKeyDeposit(), actual.getKeyDeposit());
  }

  @Test
  void testAddedHistoriesPoolDeposit() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .poolDeposit(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.POOL_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .poolDeposit(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getPoolDeposit(), actual.getPoolDeposit());
  }

  @Test
  void testAddedHistoriesMaxEpoch() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_EPOCH), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxEpoch(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxEpoch(), actual.getMaxEpoch());
  }

  @Test
  void testAddedHistoriesOptimalPoolCount() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.OPTIMAL_POOL_COUNT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .optimalPoolCount(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getOptimalPoolCount(), actual.getOptimalPoolCount());
  }

  @Test
  void testAddedHistoriesInfluence() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .influence(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.INFLUENCE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .influence(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getInfluence(), actual.getInfluence());
  }

  @Test
  void testAddedHistoriesMonetaryExpandRate() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MONETARY_EXPAND_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .monetaryExpandRate(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMonetaryExpandRate(), actual.getMonetaryExpandRate());
  }

  @Test
  void testAddedHistoriesTreasuryGrowthRate() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.TREASURY_GROWTH_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .treasuryGrowthRate(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getTreasuryGrowthRate(), actual.getTreasuryGrowthRate());
  }

  @Test
  void testAddedHistoriesDecentralisation() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.DECENTRALISATION), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .decentralisation(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getDecentralisation(), actual.getDecentralisation());
  }

  @Test
  void testAddedHistoriesEntropy() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.ENTROPY), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .entropy(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.toString())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getEntropy(), actual.getEntropy());
  }

  @Test
  void testAddedHistoriesProtocolMajor() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MAJOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMajor(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getProtocolMajor(), actual.getProtocolMajor());
  }

  @Test
  void testAddedHistoriesProtocolMinor() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MINOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMinor(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getProtocolMinor(), actual.getProtocolMinor());
  }

  @Test
  void testAddedHistoriesMinUtxoValue() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .minUtxoValue(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_UTXO_VALUE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minUtxoValue(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMinUtxoValue(), actual.getMinUtxoValue());
  }

  @Test
  void testAddedHistoriesMinPoolCost() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .minPoolCost(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_POOL_COST), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minPoolCost(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMinPoolCost(), actual.getMinPoolCost());
  }

  @Test
  void testAddedHistoriesCostModel() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COST_MODEL), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .costModel(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.toString())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getCostModel(), actual.getCostModel());
  }

  @Test
  void testAddedHistoriesPriceMem() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceMem(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getPriceMem(), actual.getPriceMem());
  }

  @Test
  void testAddedHistoriesPriceStep() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_STEP), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceStep(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.doubleValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getPriceStep(), actual.getPriceStep());
  }

  @Test
  void testAddedHistoriesMaxTxExMem() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxTxExMem(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExMem(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxExMem(), actual.getMaxTxExMem());
  }

  @Test
  void testAddedHistoriesMaxTxExSteps() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxTxExSteps(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExSteps(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxTxExSteps(), actual.getMaxTxExSteps());
  }

  @Test
  void testAddedHistoriesMaxBlockExMem() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockExMem(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExMem(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockExMem(), actual.getMaxBlockExMem());
  }

  @Test
  void testAddedHistoriesMaxBlockExSteps() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExSteps(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxBlockExSteps(), actual.getMaxBlockExSteps());
  }

  @Test
  void testAddedHistoriesMaxValSize() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxValSize(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_VAL_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxValSize(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxValSize(), actual.getMaxValSize());
  }

  @Test
  void testAddedHistoriesCollateralPercent() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COLLATERAL_PERCENT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .collateralPercent(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getCollateralPercent(), actual.getCollateralPercent());
  }

  @Test
  void testAddedHistoriesMaxCollateralInputs() {
    // protocol change
    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_COLLATERAL_INPUTS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxCollateralInputs(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE.intValue())
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getMaxCollateralInputs(), actual.getMaxCollateralInputs());
  }

  @Test
  void testAddedHistoriesCoinsPerUtxoSize() {
    // protocol change

    EpochParam epochParam = EpochParam.builder()
        .epochNo(BigInteger.ONE.intValue())
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(Collections.emptyList());
    // epoch param
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParam));

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(Collections.emptyList());

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COINS_PER_UTXO_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .coinsPerUtxoSize(List.of(ProtocolHistory.builder()
            .value(BigInteger.ONE)
            .status(ProtocolStatus.ADDED)
            .build()))
        .build();
    Assertions.assertEquals(expect.getCoinsPerUtxoSize(), actual.getCoinsPerUtxoSize());
  }

  // Last change Protocols


  @Test
  void testEmptyProposalProtocols() {
    when(paramProposalRepository.findMaxEpochChange()).thenReturn(BigInteger.ONE.intValue());

    Protocols expect = new Protocols();
    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testEmptyProtocols() {
    when(paramProposalRepository.findMaxEpochChange()).thenReturn(null);
    Protocols expect = new Protocols();
    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testChangedHistoriesMinFeeAThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeA(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_A), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeA(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinFeeA(), actual.getMinFeeA());
  }

  @Test
  void testChangedHistoriesMinFeeAThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeA(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeA(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_A), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeA(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinFeeA(), actual.getMinFeeA());
  }

  @Test
  void testChangedHistoriesMinFeeBThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minFeeB(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeB(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeB(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_B), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeB(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinFeeB(), actual.getMinFeeB());
  }

  @Test
  void testChangedHistoriesMinFeeBThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minFeeB(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minFeeB(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minFeeB(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_FEE_B), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minFeeB(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinFeeB(), actual.getMinFeeB());
  }

  @Test
  void testChangedHistoriesMaxBlockSizeThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockSize(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockSize(), actual.getMaxBlockSize());
  }

  @Test
  void testChangedHistoriesMaxBlockSizeThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockSize(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockSize(), actual.getMaxBlockSize());
  }

  @Test
  void testChangedHistoriesMaxTxSizeThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxSize(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxSize(), actual.getMaxTxSize());
  }

  @Test
  void testChangedHistoriesMaxTxSizeThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxSize(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxSize(), actual.getMaxTxSize());
  }

  @Test
  void testChangedHistoriesMaxBhSizeThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBhSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBhSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBhSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BH_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBhSize(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBhSize(), actual.getMaxBhSize());
  }

  @Test
  void testChangedHistoriesMaxBhSizeThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBhSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBhSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBhSize(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BH_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBhSize(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBhSize(), actual.getMaxBhSize());
  }

  @Test
  void testChangedHistoriesKeyDepositThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .keyDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .keyDeposit(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .keyDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.KEY_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .keyDeposit(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getKeyDeposit(), actual.getKeyDeposit());
  }

  @Test
  void testChangedHistoriesKeyDepositThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .keyDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .keyDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .keyDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.KEY_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .keyDeposit(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getKeyDeposit(), actual.getKeyDeposit());
  }

  @Test
  void testChangedHistoriesPoolDepositThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .poolDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .poolDeposit(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .poolDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.POOL_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .poolDeposit(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPoolDeposit(), actual.getPoolDeposit());
  }

  @Test
  void testChangedHistoriesPoolDepositThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .poolDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .poolDeposit(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .poolDeposit(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.POOL_DEPOSIT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .poolDeposit(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPoolDeposit(), actual.getPoolDeposit());
  }

  @Test
  void testChangedHistoriesMaxEpochThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxEpoch(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxEpoch(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxEpoch(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_EPOCH), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxEpoch(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxEpoch(), actual.getMaxEpoch());
  }

  @Test
  void testChangedHistoriesMaxEpochThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxEpoch(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxEpoch(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxEpoch(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_EPOCH), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxEpoch(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxEpoch(), actual.getMaxEpoch());
  }

  @Test
  void testChangedOptimalPoolCountThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .optimalPoolCount(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .optimalPoolCount(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .optimalPoolCount(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.OPTIMAL_POOL_COUNT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .optimalPoolCount(
            List.of(
                ProtocolHistory.builder()
                    .value(2)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getOptimalPoolCount(), actual.getOptimalPoolCount());
  }

  @Test
  void testChangedOptimalPoolCountThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .optimalPoolCount(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .optimalPoolCount(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .optimalPoolCount(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.OPTIMAL_POOL_COUNT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .optimalPoolCount(
            List.of(
                ProtocolHistory.builder()
                    .value(1)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getOptimalPoolCount(), actual.getOptimalPoolCount());
  }

  @Test
  void testChangedInfluenceThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .influence(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .influence(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .influence(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.INFLUENCE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .influence(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getInfluence(), actual.getInfluence());
  }

  @Test
  void testChangedInfluenceThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .influence(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .influence(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .influence(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.INFLUENCE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .influence(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getInfluence(), actual.getInfluence());
  }

  @Test
  void testChangedMonetaryExpandRateThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MONETARY_EXPAND_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .monetaryExpandRate(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMonetaryExpandRate(), actual.getMonetaryExpandRate());
  }

  @Test
  void testChangedMonetaryExpandRateThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MONETARY_EXPAND_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .monetaryExpandRate(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMonetaryExpandRate(), actual.getMonetaryExpandRate());
  }

  @Test
  void testChangedTreasuryGrowthRateThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.TREASURY_GROWTH_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .treasuryGrowthRate(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getTreasuryGrowthRate(), actual.getTreasuryGrowthRate());
  }

  @Test
  void testChangedTreasuryGrowthRateThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.TREASURY_GROWTH_RATE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .treasuryGrowthRate(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getTreasuryGrowthRate(), actual.getTreasuryGrowthRate());
  }

  @Test
  void testChangedDecentralisationThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .decentralisation(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .decentralisation(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .decentralisation(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.DECENTRALISATION), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .decentralisation(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getDecentralisation(), actual.getDecentralisation());
  }

  @Test
  void testChangedDecentralisationThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .decentralisation(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .decentralisation(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .decentralisation(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.DECENTRALISATION), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .decentralisation(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getDecentralisation(), actual.getDecentralisation());
  }

  @Test
  void testChangedProtocolMajorThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .protocolMajor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMajor(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMajor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MAJOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMajor(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getProtocolMajor(), actual.getProtocolMajor());
  }

  @Test
  void testChangedProtocolMajorThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .protocolMajor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMajor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMajor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MAJOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMajor(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getProtocolMajor(), actual.getProtocolMajor());
  }

  @Test
  void testChangedProtocolMinorThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .protocolMinor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMinor(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMinor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MINOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMinor(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getProtocolMinor(), actual.getProtocolMinor());
  }

  @Test
  void testChangedProtocolMinorThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .protocolMinor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .protocolMinor(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .protocolMinor(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PROTOCOL_MINOR), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .protocolMinor(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getProtocolMinor(), actual.getProtocolMinor());
  }

  @Test
  void testChangedMinUtxoValueThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minUtxoValue(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minUtxoValue(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minUtxoValue(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_UTXO_VALUE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minUtxoValue(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinUtxoValue(), actual.getMinUtxoValue());
  }

  @Test
  void testChangedMinUtxoValueThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minUtxoValue(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minUtxoValue(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minUtxoValue(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_UTXO_VALUE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minUtxoValue(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinUtxoValue(), actual.getMinUtxoValue());
  }

  @Test
  void testChangedMinPoolCostThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minPoolCost(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minPoolCost(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minPoolCost(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_POOL_COST), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minPoolCost(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinPoolCost(), actual.getMinPoolCost());
  }

  @Test
  void testChangedMinPoolCostThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .minPoolCost(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .minPoolCost(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .minUtxoValue(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MIN_POOL_COST), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .minPoolCost(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMinPoolCost(), actual.getMinPoolCost());
  }

  @Test
  void testChangedCostModelThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .costModel(BigInteger.ONE.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .costModel(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .costModel(CostModel.builder().id(2L).costs(BigInteger.TWO.toString()).build())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    when(costModelRepository.findById(any(Long.class)))
        .thenReturn(Optional.of(CostModel.builder()
            .id(2L)
            .costs("2")
            .build()));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COST_MODEL), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .costModel(
            List.of(
                ProtocolHistory.builder()
                    .value("2")
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value("1")
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(Objects.hashCode(expect.getCostModel()),
        Objects.hashCode(actual.getCostModel()));
  }

  @Test
  void testChangedCostModelThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .costModel(BigInteger.ONE.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .costModel(BigInteger.ONE.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .costModel(CostModel.builder().id(1L).costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .costModel(CostModel.builder().id(2L).costs(BigInteger.TWO.toString()).build())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    when(costModelRepository.findById(any(Long.class)))
        .thenReturn(Optional.of(CostModel.builder()
            .id(1L)
            .costs("1")
            .build()));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COST_MODEL), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .costModel(
            List.of(
                ProtocolHistory.builder()
                    .value("1")
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getCostModel(), actual.getCostModel());
  }

  @Test
  void testChangedPriceMemThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .priceMem(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceMem(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceMem(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPriceMem(), actual.getPriceMem());
  }

  @Test
  void testChangedPriceMemThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .priceMem(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceMem(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceMem(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPriceMem(), actual.getPriceMem());
  }

  @Test
  void testChangedPriceStepThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .priceStep(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceStep(BigInteger.TWO.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceStep(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_STEP), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceStep(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.doubleValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPriceStep(), actual.getPriceStep());
  }

  @Test
  void testChangedPriceStepThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .priceStep(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .priceStep(BigInteger.ONE.doubleValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .priceStep(BigInteger.TWO.doubleValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.PRICE_STEP), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .priceStep(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.doubleValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getPriceStep(), actual.getPriceStep());
  }

  @Test
  void testChangedMaxTxExMemThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExMem(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxExMem(), actual.getMaxTxExMem());
  }

  @Test
  void testChangedMaxTxExMemThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxExMem(), actual.getMaxTxExMem());
  }

  @Test
  void testChangedMaxTxExStepsThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExSteps(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExSteps(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxExSteps(), actual.getMaxTxExSteps());
  }

  @Test
  void testChangedMaxTxExStepsThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxTxExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxTxExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxTxExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_TX_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxTxExSteps(
            List.of(
                ProtocolHistory.builder()
                    .transactionHash(txTwo.getHash())
                    .time(txOne.getBlock().getTime())
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxTxExSteps(), actual.getMaxTxExSteps());
  }

  @Test
  void testChangedMaxBlockExMemThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExMem(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockExMem(), actual.getMaxBlockExMem());
  }

  @Test
  void testChangedMaxBlockExMemThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExMem(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExMem(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_MEM), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExMem(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockExMem(), actual.getMaxBlockExMem());
  }

  @Test
  void testChangedMaxBlockExStepshatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExSteps(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExSteps(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockExSteps(), actual.getMaxBlockExSteps());
  }

  @Test
  void testChangedMaxBlockExStepsThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxBlockExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxBlockExSteps(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxBlockExSteps(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_BLOCK_EX_STEPS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxBlockExSteps(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxBlockExSteps(), actual.getMaxBlockExSteps());
  }

  @Test
  void testChangedMaxValSizethatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxValSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxValSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxValSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_VAL_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxValSize(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxValSize(), actual.getMaxValSize());
  }

  @Test
  void testChangedMaxValSizeThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxValSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxValSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxValSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_VAL_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxValSize(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxValSize(), actual.getMaxValSize());
  }

  @Test
  void testChangedCollateralPercentthatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .collateralPercent(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .collateralPercent(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .collateralPercent(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COLLATERAL_PERCENT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .collateralPercent(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.intValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.intValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getCollateralPercent(), actual.getCollateralPercent());
  }

  @Test
  void testChangedCollateralPercentThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .collateralPercent(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .collateralPercent(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .collateralPercent(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COLLATERAL_PERCENT), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .collateralPercent(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.intValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getCollateralPercent(), actual.getCollateralPercent());
  }

  @Test
  void testChangedMaxCollateralInputsThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxCollateralInputs(BigInteger.TWO.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxCollateralInputs(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_COLLATERAL_INPUTS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxCollateralInputs(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO.intValue())
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.intValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxCollateralInputs(), actual.getMaxCollateralInputs());
  }

  @Test
  void testChangedMaxCollateralInputsThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .maxCollateralInputs(BigInteger.TWO.intValue())
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.MAX_COLLATERAL_INPUTS), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .maxCollateralInputs(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE.intValue())
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getMaxCollateralInputs(), actual.getMaxCollateralInputs());
  }

  @Test
  void testChangedCoinsPerUtxoSizeThatChooseThePerfectOne() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .coinsPerUtxoSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .coinsPerUtxoSize(BigInteger.TWO)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .coinsPerUtxoSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COINS_PER_UTXO_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .coinsPerUtxoSize(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.TWO)
                    .status(ProtocolStatus.UPDATED)
                    .build(),
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getCoinsPerUtxoSize(), actual.getCoinsPerUtxoSize());
  }

  @Test
  void testChangedCoinsPerUtxoSizeThatShouldNotChooseTheLatest() {

    ParamHistoryProjection protocolChangeEpochOne = ParamHistoryProjection.builder()
        .coinsPerUtxoSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.ONE.longValue())
        .build();
    ParamHistoryProjection protocolChangeEpochTwo = ParamHistoryProjection.builder()
        .coinsPerUtxoSize(BigInteger.ONE)
        .epochNo(BigInteger.TWO.intValue())
        .tx(BigInteger.TWO.longValue())
        .build();

    when(paramProposalRepository.findProtocolsChange())
        .thenReturn(List.of(protocolChangeEpochOne, protocolChangeEpochTwo));

    EpochParam epochParamOne = EpochParam.builder()
        .epochNo(1)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .epochNo(2)
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamThree = EpochParam.builder()
        .epochNo(3)
        .coinsPerUtxoSize(BigInteger.TWO)
        .build();

    //epoch
    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    // tx
    Tx txOne = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();
    Tx txTwo = Tx.builder()
        .id(BigInteger.TWO.longValue())
        .hash("12323")
        .block(Block.builder()
            .time(Timestamp.valueOf(LocalDateTime.now()))
            .build())
        .build();

    when(txRepository.findByIdIn(anyList()))
        .thenReturn(List.of(txOne, txTwo));

    HistoriesProtocol actual = protocolParamService.getHistoryProtocolParameters(
        List.of(ProtocolType.COINS_PER_UTXO_SIZE), null, null);

    HistoriesProtocol expect = HistoriesProtocol.builder()
        .coinsPerUtxoSize(
            List.of(
                ProtocolHistory.builder()
                    .value(BigInteger.ONE)
                    .status(ProtocolStatus.ADDED)
                    .build()
            )
        )
        .build();
    Assertions.assertEquals(expect.getCoinsPerUtxoSize(), actual.getCoinsPerUtxoSize());
  }

}

