/*
package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;



import org.cardanofoundation.explorer.api.common.enumeration.ProtocolType;
import org.cardanofoundation.explorer.api.model.response.protocol.ProtocolHistory;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.ParamHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.CostModel;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class ProtocolServiceTest {

  @Mock
  ParamProposalRepository paramProposalRepository;
  @Mock
  EpochParamRepository epochParamRepository;
  @Mock
  TxRepository txRepository;

  @InjectMocks
  private ProtocolParamServiceImpl protocolParamService;

  @BeforeEach
  void setup() {
    protocolParamService.setup();
  }

  private Timestamp getTimeStamp(long days) {
    return Timestamp.valueOf(LocalDateTime.now().minusDays(days));
  }

  @Test
  void testCurrentProtocolDontHaveOldValue() {
    List<ParamProposal> paramProposals = new ArrayList<>();

    EpochParam epochParam = EpochParam.builder()
        .epochNo(1)
        .minFeeA(2).minFeeB(3).maxBlockSize(4).maxTxSize(5)
        .maxBhSize(6).keyDeposit(BigInteger.valueOf(7L)).poolDeposit(BigInteger.valueOf(8))
        .maxEpoch(9).optimalPoolCount(10).influence(9.0).monetaryExpandRate(10.0)
        .treasuryGrowthRate(11.0).decentralisation(12.0)
        .extraEntropy("Test history").protocolMajor(13).protocolMinor(14)
        .minUtxoValue(BigInteger.valueOf(15)).minPoolCost(BigInteger.valueOf(16))
        .coinsPerUtxoSize(BigInteger.valueOf(17)).priceMem(18.0).priceStep(19.0)
        .costModel(CostModel.builder().hash("hash").costs("cost").build())
        .maxTxExMem(BigInteger.valueOf(20)).maxTxExSteps(BigInteger.valueOf(21))
        .maxBlockExMem(BigInteger.valueOf(22)).maxBlockExSteps(BigInteger.valueOf(23))
        .maxValSize(BigInteger.valueOf(24)).collateralPercent(25).maxCollateralInputs(26)
        .build();

    List<String> hashes = IntStream.range(0, 29).boxed().map(String::valueOf)
        .collect(Collectors.toList());

    List<Tx> txs = new ArrayList<>();
    txs.add(Tx.builder().id(30L).hash(hashes.get(28))
        .block(Block.builder().time(getTimeStamp(0L)).build()).build());

    LongStream.range(0, 29).boxed()
        .forEach(id -> txs.add(Tx.builder().id(29L - id).hash(hashes.get(id.intValue()))
            .block(Block.builder().time(getTimeStamp(id + 1)).build()).build()));

    List<ParamHistory> paramHistories = new ArrayList<>();
    var time = Timestamp.valueOf(LocalDateTime.now());
    // empty history
    paramHistories.add(
        ParamHistoryProjection.builder().id(30L).hash(hashes.get(28))
            .time(txs.get(0).getBlock().getTime())
            .tx(30L).build());
    //minFeeA
    paramHistories.add(
        ParamHistoryProjection.builder().id(29L).minFeeA(BigInteger.valueOf(2))
            .hash(hashes.get(0))
            .tx(29L)
            .time(txs.get(1).getBlock().getTime()).build());
    //minFeeB
    paramHistories.add(
        ParamHistoryProjection.builder().id(28L).minFeeB(BigInteger.valueOf(3))
            .hash(hashes.get(1))
            .tx(28L)
            .time(txs.get(2).getBlock().getTime()).build());
    //maxBlockSize
    paramHistories.add(
        ParamHistoryProjection.builder().id(27L).maxBlockSize(BigInteger.valueOf(4))
            .hash(hashes.get(2))
            .tx(27L)
            .time(txs.get(3).getBlock().getTime()).build());
    //maxTxSize
    paramHistories.add(
        ParamHistoryProjection.builder().id(26L).maxTxSize(BigInteger.valueOf(5))
            .hash(hashes.get(3))
            .tx(26L)
            .time(txs.get(4).getBlock().getTime()).build());
    //maxBhSize
    paramHistories.add(
        ParamHistoryProjection.builder().id(25L)
            .maxBhSize(BigInteger.valueOf(6))
            .hash(hashes.get(4))
            .tx(25L)
            .time(txs.get(5).getBlock().getTime()).build());
    //keyDeposit
    paramHistories.add(
        ParamHistoryProjection.builder()
            .keyDeposit(BigInteger.valueOf(7L))
            .id(23L).hash(hashes.get(5))
            .tx(24L)
            .time(txs.get(6).getBlock().getTime()).build());
    //poolDeposit
    paramHistories.add(
        ParamHistoryProjection.builder().id(22L)
            .poolDeposit(BigInteger.valueOf(8))
            .hash(hashes.get(6))
            .tx(23L)
            .time(txs.get(7).getBlock().getTime()).build());
    //maxEpoch
    paramHistories.add(
        ParamHistoryProjection.builder().id(21L)
            .maxEpoch(BigInteger.valueOf(9))
            .hash(hashes.get(7))
            .tx(22L)
            .time(txs.get(8).getBlock().getTime()).build());
    //optimalPoolCount
    paramHistories.add(
        ParamHistoryProjection.builder().id(20L)
            .optimalPoolCount(BigInteger.valueOf(10))
            .hash(hashes.get(8))
            .tx(21L)
            .time(txs.get(9).getBlock().getTime()).build());
    //influence
    paramHistories.add(
        ParamHistoryProjection.builder().id(19L)
            .influence(9.0)
            .tx(20L)
            .hash(hashes.get(9))
            .time(txs.get(10).getBlock().getTime()).build());
    //monetaryExpandRate
    paramHistories.add(
        ParamHistoryProjection.builder().id(18L)
            .monetaryExpandRate(10.0)
            .tx(19L)
            .hash(hashes.get(10))
            .time(txs.get(11).getBlock().getTime()).build());
    //treasuryGrowthRate
    paramHistories.add(
        ParamHistoryProjection.builder().id(17L)
            .treasuryGrowthRate(11.0)
            .hash(hashes.get(11))
            .tx(18L)
            .time(txs.get(12).getBlock().getTime()).build());
    //decentralisation
    paramHistories.add(
        ParamHistoryProjection.builder().id(16L)
            .decentralisation(12.0)
            .tx(17L)
            .hash(hashes.get(12))
            .time(txs.get(13).getBlock().getTime()).build());
    //extraEntropy
    paramHistories.add(
        ParamHistoryProjection.builder().id(15L)
            .extraEntropy("Test history")
            .tx(16L)
            .hash(hashes.get(13))
            .time(txs.get(14).getBlock().getTime()).build());
    //protocolMajor
    paramHistories.add(
        ParamHistoryProjection.builder().id(14L)
            .protocolMajor(13)
            .tx(15L)
            .hash(hashes.get(14))
            .time(txs.get(15).getBlock().getTime()).build());
    //protocolMinor
    paramHistories.add(
        ParamHistoryProjection.builder().id(13L)
            .tx(14L)
            .protocolMinor(14)
            .hash(hashes.get(15))
            .time(txs.get(16).getBlock().getTime()).build());
    //minUtxoValue
    paramHistories.add(
        ParamHistoryProjection.builder().id(12L)
            .tx(13L)
            .minUtxoValue(BigInteger.valueOf(15))
            .hash(hashes.get(16))
            .time(txs.get(17).getBlock().getTime()).build());
    //minPoolCost
    paramHistories.add(
        ParamHistoryProjection.builder().id(11L)
            .tx(12L)
            .minPoolCost(BigInteger.valueOf(16))
            .hash(hashes.get(17))
            .time(txs.get(18).getBlock().getTime()).build());
    //coinsPerUtxoSize
    paramHistories.add(
        ParamHistoryProjection.builder().id(10L)
            .tx(11L)
            .coinsPerUtxoSize(BigInteger.valueOf(17))
            .hash(hashes.get(18))
            .time(txs.get(19).getBlock().getTime()).build());
    //costModel
    paramHistories.add(
        ParamHistoryProjection.builder().id(9L)
            .tx(10L)
            .costModel(1L)
            .hash(hashes.get(19))
            .time(txs.get(20).getBlock().getTime()).build());
    //priceMem
    paramHistories.add(
        ParamHistoryProjection.builder().id(8L)
            .tx(9L)
            .priceMem(18.0)
            .hash(hashes.get(20))
            .time(txs.get(21).getBlock().getTime()).build());
    //priceStep
    paramHistories.add(
        ParamHistoryProjection.builder().id(7L)
            .tx(8L)
            .priceStep(19.0)
            .hash(hashes.get(21))
            .time(txs.get(22).getBlock().getTime()).build());
    //maxTxExMem
    paramHistories.add(
        ParamHistoryProjection.builder().id(6L)
            .tx(7L)
            .maxTxExMem(BigInteger.valueOf(20))
            .hash(hashes.get(22))
            .time(txs.get(23).getBlock().getTime()).build());
    //maxTxExSteps
    paramHistories.add(
        ParamHistoryProjection.builder().id(5L)
            .tx(6L)
            .maxTxExSteps(BigInteger.valueOf(21))
            .hash(hashes.get(23))
            .time(txs.get(24).getBlock().getTime()).build());
    //maxBlockExMem
    paramHistories.add(
        ParamHistoryProjection.builder().id(4L)
            .tx(5L)
            .maxBlockExMem(BigInteger.valueOf(22))
            .hash(hashes.get(24))
            .time(txs.get(25).getBlock().getTime()).build());
    //maxBlockExSteps
    paramHistories.add(
        ParamHistoryProjection.builder().id(3L)
            .tx(4L)
            .maxBlockExSteps(BigInteger.valueOf(23))
            .hash(hashes.get(25))
            .time(txs.get(26).getBlock().getTime()).build());
    //maxValSize
    paramHistories.add(
        ParamHistoryProjection.builder().id(2L)
            .tx(3L)
            .hash(hashes.get(26))
            .maxValSize(BigInteger.valueOf(24))
            .time(txs.get(27).getBlock().getTime()).build());
    //collateralPercent
    paramHistories.add(
        ParamHistoryProjection.builder().id(1L)
            .tx(2L)
            .hash(hashes.get(27))
            .collateralPercent(25)
            .maxCollateralInputs(26)
            .time(txs.get(28).getBlock().getTime()).build());

    when(epochParamRepository.findTopEpochParam()).thenReturn(epochParam);
    when(paramProposalRepository.findProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);
    when(txRepository.findByIdIn(anyList())).thenReturn(txs);

    var minFeeA = paramHistories.get(1);
    var minFeeB = paramHistories.get(2);
    var maxBlockSize = paramHistories.get(3);
    var maxTxSize = paramHistories.get(4);
    var maxBhSize = paramHistories.get(5);
    var keyDeposit = paramHistories.get(6);
    var poolDeposit = paramHistories.get(7);
    var maxEpoch = paramHistories.get(8);
    var optimalPoolCount = paramHistories.get(9);
    var influence = paramHistories.get(10);
    var monetaryExpandRate = paramHistories.get(11);
    var treasuryGrowthRate = paramHistories.get(12);
    var decentralisation = paramHistories.get(13);
    var extraEntropy = paramHistories.get(14);
    var protocolMajor = paramHistories.get(15);
    var protocolMinor = paramHistories.get(16);
    var minUtxoValue = paramHistories.get(17);
    var minPoolCost = paramHistories.get(18);
    var coinsPerUtxoSize = paramHistories.get(19);
    var costModel = paramHistories.get(20);
    var priceMem = paramHistories.get(21);
    var priceStep = paramHistories.get(22);
    var maxTxExMem = paramHistories.get(23);
    var maxTxExSteps = paramHistories.get(24);
    var maxBlockExMem = paramHistories.get(25);
    var maxBlockExSteps = paramHistories.get(26);
    var maxValSize = paramHistories.get(27);
    var collateralPercent = paramHistories.get(28);

    Protocols expectProtocols = Protocols.builder()
        .minFeeA(getBuildHistory(minFeeA, minFeeA.getMinFeeA(),new ProtocolHistory()))
        .minFeeB(getBuildHistory(minFeeB, minFeeB.getMinFeeB(),new ProtocolHistory()))
        .maxBlockSize(getBuildHistory(maxBlockSize, maxBlockSize.getMaxBlockSize(),new ProtocolHistory()))
        .maxTxSize(getBuildHistory(maxTxSize, maxTxSize.getMaxTxSize(),new ProtocolHistory()))
        .maxBhSize(getBuildHistory(maxBhSize, maxBhSize.getMaxBhSize(),new ProtocolHistory()))
        .keyDeposit(getBuildHistory(keyDeposit, keyDeposit.getKeyDeposit(),new ProtocolHistory()))
        .poolDeposit(getBuildHistory(poolDeposit, poolDeposit.getPoolDeposit(),new ProtocolHistory()))
        .maxEpoch(getBuildHistory(maxEpoch, maxEpoch.getMaxEpoch(),new ProtocolHistory()))
        .optimalPoolCount(getBuildHistory(optimalPoolCount, optimalPoolCount.getOptimalPoolCount(),new ProtocolHistory()))
        .influence(getBuildHistory(influence, influence.getInfluence(),new ProtocolHistory()))
        .monetaryExpandRate(
            getBuildHistory(monetaryExpandRate, monetaryExpandRate.getMonetaryExpandRate(),new ProtocolHistory()))
        .treasuryGrowthRate(
            getBuildHistory(treasuryGrowthRate, treasuryGrowthRate.getTreasuryGrowthRate(),new ProtocolHistory()))
        .decentralisation(getBuildHistory(decentralisation, decentralisation.getDecentralisation(),new ProtocolHistory()))
        .entropy(getBuildHistory(extraEntropy, extraEntropy.getEntropy(),new ProtocolHistory()))
        .protocolMajor(getBuildHistory(protocolMajor, protocolMajor.getProtocolMajor(),new ProtocolHistory()))
        .protocolMinor(getBuildHistory(protocolMinor, protocolMinor.getProtocolMinor(),new ProtocolHistory()))
        .minUtxoValue(getBuildHistory(minUtxoValue, minUtxoValue.getMinUtxoValue(),new ProtocolHistory()))
        .minPoolCost(getBuildHistory(minPoolCost, minPoolCost.getMinPoolCost(),new ProtocolHistory()))
        .coinsPerUtxoSize(getBuildHistory(coinsPerUtxoSize, coinsPerUtxoSize.getCoinsPerUtxoSize(),new ProtocolHistory()))
        .priceMem(getBuildHistory(priceMem, priceMem.getPriceMem(),new ProtocolHistory()))
        .priceStep(getBuildHistory(priceStep, priceStep.getPriceStep(),new ProtocolHistory()))
        .maxTxExMem(getBuildHistory(maxTxExMem, maxTxExMem.getMaxTxExMem(),new ProtocolHistory()))
        .maxTxExSteps(getBuildHistory(maxTxExSteps, maxTxExSteps.getMaxTxExSteps(),new ProtocolHistory()))
        .maxBlockExMem(getBuildHistory(maxBlockExMem, maxBlockExMem.getMaxBlockExMem(),new ProtocolHistory()))
        .maxBlockExSteps(getBuildHistory(maxBlockExSteps, maxBlockExSteps.getMaxBlockExSteps(),new ProtocolHistory()))
        .maxValSize(getBuildHistory(maxValSize, maxValSize.getMaxValSize(),new ProtocolHistory()))
        .collateralPercent(
            getBuildHistory(collateralPercent, collateralPercent.getCollateralPercent(),new ProtocolHistory()))
        .build();
    expectProtocols.setMaxCollateralInputs(
        getBuildHistory(collateralPercent, collateralPercent.getMaxCollateralInputs(),new ProtocolHistory()));
    expectProtocols.setCostModel(getBuildHistory(costModel, epochParam.getCostModel().getCosts(),new ProtocolHistory()));

    Protocols actualProtocols = protocolParamService.getProtocolCurrentHistory();
    Assertions.assertEquals(expectProtocols.toString(), actualProtocols.toString());
  }

  private static ProtocolHistory getBuildHistory(ParamHistory minFeeA, Object value, ProtocolHistory oldValue) {
    return ProtocolHistory.builder().value(value).transactionHash(minFeeA.getHash())
        .time(minFeeA.getTime())
        .oldValue(oldValue).build();
  }

  @Test
  void testGetProtocolOrderHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    Set<ProtocolHistory> expect = new LinkedHashSet<>();

    final int start = 200;
    for (int i = start; i > 0; i--) {
      var time = Timestamp.valueOf(LocalDateTime.now().minusHours(start - i));
      paramProposals.add(
          ParamProposal.builder().minFeeA(BigInteger.valueOf(i))
              .registeredTx(Tx.builder()
                  .id(time.getTime() - (start - i))
                  .block(Block.
                      builder()
                      .time(time)
                      .build())
                  .hash(String.valueOf(time))
                  .build())
              .build()
      );

      expect.add(ProtocolHistory.builder()
          .value(BigInteger.valueOf(i))
          .transactionHash(String.valueOf(time))
          .time(time)
          .build());
    }

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class)))
        .thenReturn(paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MIN_FEE_A);

    Assertions.assertEquals(response, expect);
  }

  @Test
  void testGetProtocolCostModelOrderHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    Set<ProtocolHistory> expect = new LinkedHashSet<>();

    final int start = 200;
    for (int i = start; i > 0; i--) {
      var time = Timestamp.valueOf(LocalDateTime.now().minusHours(start - i));
      paramProposals.add(
          ParamProposal.builder().
              costModel(CostModel.builder()
                  .costs(String.valueOf(time))
                  .hash(String.valueOf(time))
                  .build())
              .registeredTx(Tx.builder()
                  .id(time.getTime() - (start - i))
                  .block(Block.
                      builder()
                      .time(time)
                      .build())
                  .hash(String.valueOf(time))
                  .build())
              .build()
      );

      expect.add(ProtocolHistory.builder()
          .value(String.valueOf(time))
          .transactionHash(String.valueOf(time))
          .time(time)
          .build());
    }

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class)))
        .thenReturn(paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.COST_MODEL);

    Assertions.assertEquals(response, expect);
  }

  @Test
  void testGetMinFeeAProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    paramProposals.add(
        ParamProposal.builder().minFeeA(BigInteger.ONE)
            .registeredTx(Tx.builder()
                .block(Block.
                    builder()
                    .time(Timestamp.valueOf(LocalDateTime.now()))
                    .build())
                .hash(String.valueOf(Timestamp.valueOf(LocalDateTime.now())))
                .build())
            .build()
    );

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MIN_FEE_A);
    Assertions.assertEquals(response.size(), BigInteger.ONE.intValue());
  }

  @Test
  void testGetMinFeeBProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    paramProposals.add(
        ParamProposal.builder().minFeeB(BigInteger.ONE)
            .registeredTx(Tx.builder()
                .block(Block.
                    builder()
                    .time(Timestamp.valueOf(LocalDateTime.now()))
                    .build())
                .hash(String.valueOf(Timestamp.valueOf(LocalDateTime.now())))
                .build())
            .build()
    );

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MIN_FEE_B);
    Assertions.assertEquals(response.size(), BigInteger.ONE.intValue());
  }

  @Test
  void testGetMaxBlockSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBhSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BH_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetKeyDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.KEY_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPoolDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.POOL_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxEpochProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MAX_EPOCH);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetOptimalPoolCountProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.OPTIMAL_POOL_COUNT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMinUtxoValueProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MIN_UTXO_VALUE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMinPoolCostProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MIN_POOL_COST);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBlockExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBlockExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxValSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_VAL_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetCoinsPerUtxoSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.COINS_PER_UTXO_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetInfluenceProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.INFLUENCE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMonetaryExpandRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MONETARY_EXPAND_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetTreasuryGrowthRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.TREASURY_GROWTH_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetDecentralisationProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.DECENTRALISATION);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPriceMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.PRICE_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPriceStepProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PRICE_STEP);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetProtocolMajorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PROTOCOL_MAJOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetProtocolMinorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PROTOCOL_MINOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetCollateralPercentProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.COLLATERAL_PERCENT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxCollateralInputsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_COLLATERAL_INPUTS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEntropyProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.ENTROPY);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  // check empty
  @Test
  void testGetEmptyMinFeeAProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MIN_FEE_A);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinFeeBProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MIN_FEE_B);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBhSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BH_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }


  @Test
  void testGetEmptyKeyDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.KEY_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }


  @Test
  void testGetEmptyPoolDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.POOL_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxEpochProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.MAX_EPOCH);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyOptimalPoolCountProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.OPTIMAL_POOL_COUNT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinUtxoValueProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MIN_UTXO_VALUE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinPoolCostProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MIN_POOL_COST);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_TX_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxValSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_VAL_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyCoinsPerUtxoSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.COINS_PER_UTXO_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyInfluenceProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.INFLUENCE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMonetaryExpandRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MONETARY_EXPAND_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyTreasuryGrowthRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.TREASURY_GROWTH_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyDecentralisationProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.DECENTRALISATION);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyPriceMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.PRICE_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyPriceStepProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PRICE_STEP);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyProtocolMajorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PROTOCOL_MAJOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyProtocolMinorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.PROTOCOL_MINOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyCollateralPercentProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.COLLATERAL_PERCENT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxCollateralInputsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(
        ProtocolType.MAX_COLLATERAL_INPUTS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyEntropyProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getSingleProtocolHistory(ProtocolType.ENTROPY);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

}
*/
