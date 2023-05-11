
package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.cardanofoundation.explorer.api.model.response.protocol.EpochChange;
import org.cardanofoundation.explorer.api.model.response.protocol.Protocols;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.api.repository.CostModelRepository;
import org.cardanofoundation.explorer.api.repository.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.ParamHistoryProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.CostModel;
import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl.getChangeProtocol;
import static org.cardanofoundation.explorer.api.service.impl.ProtocolParamServiceImpl.mapProtocols;
import static org.mockito.ArgumentMatchers.any;
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
  @Mock
  CostModelRepository costModelRepository;

  private final static int MAX_EPOCH_LATEST = 3;

  @InjectMocks
  private ProtocolParamServiceImpl protocolParamService;

  @BeforeEach
  void setup() {
    protocolParamService.setup();
  }

  private Timestamp getTimeStamp(long days) {
    return Timestamp.valueOf(LocalDateTime.now().minusDays(days));
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

  // History Protocols
  /*@Test
  void testLatestMinFeeA() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(MAX_EPOCH_LATEST);

    EpochParam epochParamOne = getBuildEpochParam(BigInteger.ZERO);
    epochParamOne.setMinFeeA(BigInteger.ZERO.intValue());

    EpochParam epochParamTwo = getBuildEpochParam(BigInteger.ONE);
    epochParamTwo.setMinFeeA(BigInteger.ONE.intValue());

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);
    epochParamThree.setMinFeeA(BigInteger.TWO.intValue());

    when(epochParamRepository.findAll())
        .thenReturn(List.of(epochParamOne, epochParamTwo, epochParamThree));

    ParamHistoryProjection paramProposalOne = getBuildParamHistory(BigInteger.ZERO);
    paramProposalOne.setMinFeeA(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = getBuildParamHistory(BigInteger.ONE);
    paramProposalTwo.setMinFeeA(BigInteger.TWO);


    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(BigInteger.TWO.intValue()))
        .thenReturn(paramHistories);
    
    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());
    
    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();
    
    when(txRepository.findByIdIn(List.of(BigInteger.ZERO.longValue(),
                                         BigInteger.ONE.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = Protocols.builder()
        .startEpoch(BigInteger.TWO.intValue())
        .endEpoch(BigInteger.TWO.intValue())
        .build();

    expect.setMinFeeA(getChangeProtocol(BigInteger.TWO.intValue()));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }*/

  // Last change Protocols
  @Test
  void testLatestMinFeeA() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMinFeeA(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMinFeeA(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMinFeeA(getChangeProtocol(BigInteger.TWO.intValue(),
                                        txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMinFeeB() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMinFeeB(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMinFeeB(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMinFeeB(getChangeProtocol(BigInteger.TWO.intValue(),
                                        txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxBlockSize() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxBlockSize(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxBlockSize(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxBlockSize(getChangeProtocol(BigInteger.TWO.intValue(),
                                             txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxTxSize() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxTxSize(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxTxSize(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxTxSize(getChangeProtocol(BigInteger.TWO.intValue(),
                                          txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxBhSize() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxBhSize(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxBhSize(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxBhSize(getChangeProtocol(BigInteger.TWO.intValue(),
                                          txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestKeyDeposit() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setKeyDeposit(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setKeyDeposit(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setKeyDeposit(getChangeProtocol(BigInteger.TWO.intValue(),
                                           txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestPoolDeposit() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setPoolDeposit(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setPoolDeposit(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setPoolDeposit(getChangeProtocol(BigInteger.TWO.intValue(),
                                            txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxEpoch() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxEpoch(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxEpoch(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxEpoch(getChangeProtocol(BigInteger.TWO.intValue(),
                                         txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestOptimalPoolCount() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setOptimalPoolCount(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setOptimalPoolCount(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setOptimalPoolCount(getChangeProtocol(BigInteger.TWO.intValue(),
                                                 txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestInfluence() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setInfluence(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setInfluence(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setInfluence(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                          txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMonetaryExpandRate() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMonetaryExpandRate(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMonetaryExpandRate(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMonetaryExpandRate(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                                   txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestTreasuryGrowthRate() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setTreasuryGrowthRate(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setTreasuryGrowthRate(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setTreasuryGrowthRate(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                                   txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestDecentralisation() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setDecentralisation(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setDecentralisation(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setDecentralisation(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                                 txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestExtraEntropy() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setExtraEntropy(BigInteger.ONE.toString());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setExtraEntropy(BigInteger.TWO.toString());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setEntropy(getChangeProtocol(BigInteger.TWO.toString(),
                                        txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestProtocolMajor() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setProtocolMajor(BigInteger.ONE.intValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setProtocolMajor(BigInteger.TWO.intValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setProtocolMajor(getChangeProtocol(BigInteger.TWO.intValue(),
                                              txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestProtocolMinor() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setProtocolMinor(BigInteger.ONE.intValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setProtocolMinor(BigInteger.TWO.intValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setProtocolMinor(getChangeProtocol(BigInteger.TWO.intValue(),
                                              txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMinUtxoValue() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMinUtxoValue(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMinUtxoValue(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMinUtxoValue(getChangeProtocol(BigInteger.TWO.intValue(),
                                             txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMinPoolCost() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMinPoolCost(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMinPoolCost(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMinPoolCost(getChangeProtocol(BigInteger.TWO.intValue(),
                                            txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestPriceMem() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setPriceMem(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setPriceMem(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setPriceMem(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                         txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestPriceStep() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setPriceStep(BigInteger.ONE.doubleValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setPriceStep(BigInteger.TWO.doubleValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setPriceStep(getChangeProtocol(BigInteger.TWO.doubleValue(),
                                          txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxTxExMem() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxTxExMem(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxTxExMem(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxTxExMem(getChangeProtocol(BigInteger.TWO.intValue(),
                                           txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxTxExSteps() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxTxExSteps(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxTxExSteps(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxTxExSteps(getChangeProtocol(BigInteger.TWO.intValue(),
                                             txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxBlockExMem() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxBlockExMem(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxBlockExMem(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxBlockExMem(getChangeProtocol(BigInteger.TWO.intValue(),
                                              txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxBlockExSteps() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxBlockExSteps(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxBlockExSteps(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxBlockExSteps(getChangeProtocol(BigInteger.TWO.intValue(),
                                                txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxValSize() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxValSize(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxValSize(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxValSize(getChangeProtocol(BigInteger.TWO.intValue(),
                                           txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestCollateralPercent() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setCollateralPercent(BigInteger.ONE.intValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setCollateralPercent(BigInteger.TWO.intValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setCollateralPercent(getChangeProtocol(BigInteger.TWO.intValue(),
                                                  txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestMaxCollateralInputs() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setMaxCollateralInputs(BigInteger.ONE.intValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setMaxCollateralInputs(BigInteger.TWO.intValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setMaxCollateralInputs(getChangeProtocol(BigInteger.TWO.intValue(),
                                                    txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestCoinsPerUtxoSize() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setCoinsPerUtxoSize(BigInteger.ONE);

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setCoinsPerUtxoSize(BigInteger.TWO);

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setCoinsPerUtxoSize(getChangeProtocol(BigInteger.TWO.intValue(),
                                                 txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testLatestCostModel() {

    CostModel costModel = CostModel.builder()
        .id(BigInteger.TWO.longValue())
        .costs(BigInteger.TWO.toString())
        .build();

    when(costModelRepository.findById(any(Long.class)))
        .thenReturn(Optional.of(costModel));

    when(paramProposalRepository.findMaxEpoch()).thenReturn(BigInteger.TWO.intValue());

    ParamHistoryProjection paramProposalOne = ParamHistoryProjection.builder()
        .id(BigInteger.ONE.longValue())
        .tx(BigInteger.ZERO.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalOne.setCostModel(BigInteger.ONE.longValue());

    ParamHistoryProjection paramProposalTwo = ParamHistoryProjection.builder()
        .id(BigInteger.TWO.longValue())
        .tx(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .build();

    paramProposalTwo.setCostModel(BigInteger.TWO.longValue());

    List<ParamHistory> paramHistories = new ArrayList<>();
    paramHistories.add(paramProposalOne);
    paramHistories.add(paramProposalTwo);

    when(paramProposalRepository
             .findEpochProtocolsChange(any(Integer.class)))
        .thenReturn(paramHistories);

    EpochParam epochParamThree = getBuildEpochParam(BigInteger.TWO);

    when(epochParamRepository.findEpochParamByEpochNo(BigInteger.TWO.intValue()))
        .thenReturn(Optional.of(epochParamThree));

    final Timestamp txOneDate = Timestamp.valueOf(LocalDateTime.now());
    final Timestamp txTwoDate = Timestamp.valueOf(LocalDateTime.now());

    Tx txOne = Tx.builder()
        .id(BigInteger.ZERO.longValue())
        .hash(BigInteger.ZERO.toString())
        .block(Block.builder()
                   .time(txOneDate)
                   .build())
        .build();

    Tx txTwo = Tx.builder()
        .id(BigInteger.ONE.longValue())
        .hash(BigInteger.ONE.toString())
        .block(Block.builder()
                   .time(txTwoDate)
                   .build())
        .build();

    when(txRepository.findByIdIn(List.of(BigInteger.ONE.longValue(), BigInteger.ZERO.longValue())))
        .thenReturn(List.of(txOne, txTwo));

    Protocols expect = mapProtocols(epochParamThree);

    expect.setEpochChange(EpochChange.builder()
                              .startEpoch(BigInteger.TWO.intValue())
                              .endEpoch(BigInteger.TWO.intValue())
                              .build());

    expect.setCostModel(getChangeProtocol(BigInteger.TWO.toString(),
                                          txTwo));

    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testEmptyProtocols() {
    when(paramProposalRepository.findMaxEpoch()).thenReturn(null);
    Protocols expect = new Protocols();
    Protocols actual = protocolParamService.getLatestChange();
    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  // Not Fixed Unit Test
  @Test
  void testNotFixedMinFeeA() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minFeeA(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMinFeeB() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minFeeB(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMaxBlockSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBlockSize(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMaxTxSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxTxSize(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMaxBhSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBhSize(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedKeyDeposit() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .keyDeposit(BigInteger.TWO)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedPoolDeposit() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .poolDeposit(BigInteger.TWO)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMaxEpoch() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxEpoch(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedOptimalPoolCount() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .optimalPoolCount(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedInfluence() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .influence(BigInteger.TWO.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(
            EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                .endEpoch(BigInteger.TWO.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMonetaryExpandRate() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .monetaryExpandRate(BigInteger.TWO.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedTreasuryGrowthRate() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .treasuryGrowthRate(BigInteger.TWO.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedDecentralisation() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .decentralisation(BigInteger.TWO.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedEntropy() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .extraEntropy(BigInteger.TWO.toString())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedProtocolMajor() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .protocolMajor(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedProtocolMinor() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .protocolMinor(BigInteger.TWO.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testNotFixedMinUtxoValue() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minUtxoValue(BigInteger.TWO)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  // Fixed Protocol Unit Test
  @Test
  void testFixedMinFeeA() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minFeeA(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .minFeeA(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMinFeeB() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minFeeB(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .minFeeB(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxBlockSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBlockSize(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxBlockSize(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxTxSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxTxSize(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxTxSize(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxBhSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBhSize(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxBhSize(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedKeyDeposit() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .keyDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .keyDeposit(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .keyDeposit(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedPoolDeposit() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .poolDeposit(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .poolDeposit(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .poolDeposit(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxEpoch() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxEpoch(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxEpoch(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedOptimalPoolCount() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .optimalPoolCount(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .optimalPoolCount(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedInfluence() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .influence(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .influence(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .influence(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMonetaryExpandRate() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .monetaryExpandRate(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .monetaryExpandRate(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedTreasuryGrowthRate() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .treasuryGrowthRate(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .treasuryGrowthRate(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedDecentralisation() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .decentralisation(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .decentralisation(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedEntropy() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .extraEntropy(BigInteger.ONE.toString())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .entropy(getChangeProtocol(BigInteger.ONE.toString()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedProtocolMajor() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .protocolMajor(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .protocolMajor(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedProtocolMinor() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .protocolMinor(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .protocolMinor(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMinUtxoValue() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minUtxoValue(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minUtxoValue(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .minUtxoValue(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMinPoolCost() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .minPoolCost(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .minPoolCost(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .minPoolCost(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedCostModel() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .costModel(CostModel.builder().costs(BigInteger.ONE.toString()).build())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .costModel(CostModel.builder().costs(BigInteger.ONE.toString()).build())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .costModel(getChangeProtocol(BigInteger.ONE.toString()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedPriceMem() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .priceMem(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .priceMem(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedPriceStep() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .priceStep(BigInteger.ONE.doubleValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .priceStep(getChangeProtocol(BigInteger.ONE.doubleValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxTxExMem() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxTxExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxTxExMem(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxTxExMem(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxTxExSteps() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxTxExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxTxExSteps(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxTxExSteps(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxBlockExMem() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockExMem(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBlockExMem(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxBlockExMem(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxBlockExSteps() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxBlockExSteps(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxBlockExSteps(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxValSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxValSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxValSize(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxValSize(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedCollateralPercent() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .collateralPercent(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .collateralPercent(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedMaxCollateralInputs() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .maxCollateralInputs(BigInteger.ONE.intValue())
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .maxCollateralInputs(getChangeProtocol(BigInteger.ONE.intValue()))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testFixedCoinsPerUtxoSize() {
    EpochParam epochParamOne = EpochParam.builder()
        .id(BigInteger.ONE.longValue())
        .epochNo(BigInteger.ONE.intValue())
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    EpochParam epochParamTwo = EpochParam.builder()
        .id(BigInteger.TWO.longValue())
        .epochNo(BigInteger.TWO.intValue())
        .coinsPerUtxoSize(BigInteger.ONE)
        .build();

    when(epochParamRepository.findAll()).thenReturn(List.of(epochParamOne, epochParamTwo));

    Protocols expect = Protocols.builder()
        .epochChange(EpochChange.builder().startEpoch(BigInteger.TWO.intValue())
                         .endEpoch(BigInteger.ONE.intValue()).build())
        .coinsPerUtxoSize(getChangeProtocol(BigInteger.ONE))
        .build();

    Protocols actual = protocolParamService.getFixedProtocols();

    Assertions.assertEquals(expect.hashCode(), actual.hashCode());
  }

  @Test
  void testEmptyFixedProtocols() {
    when(epochParamRepository.findAll()).thenReturn(Collections.emptyList());

    Protocols protocols = protocolParamService.getFixedProtocols();
    Assertions.assertEquals(protocols.hashCode(), new Protocols().hashCode());
  }

}

