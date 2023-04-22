package com.cardano.explorer.protocol;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import com.cardano.explorer.repository.ParamProposalRepository;
import com.cardano.explorer.service.impl.ProtocolParamServiceImpl;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.CostModel;
import com.sotatek.cardano.common.entity.ParamProposal;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.ledgersync.util.JsonUtil;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProtocolServiceTest {

  @Mock
  ParamProposalRepository paramProposalRepository;

  @InjectMocks
  private ProtocolParamServiceImpl protocolParamService;

  @BeforeEach
  void setup() {
    protocolParamService.setup();
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

    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MIN_FEE_A);

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

    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
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

    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MIN_FEE_A);
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

    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MIN_FEE_B);
    Assertions.assertEquals(response.size(), BigInteger.ONE.intValue());
  }

  @Test
  void testGetMaxBlockSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBhSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BH_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetKeyDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.KEY_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPoolDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.POOL_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxEpochProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MAX_EPOCH);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetOptimalPoolCountProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.OPTIMAL_POOL_COUNT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMinUtxoValueProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MIN_UTXO_VALUE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMinPoolCostProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MIN_POOL_COST);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxTxExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBlockExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxBlockExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxValSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_VAL_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetCoinsPerUtxoSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.COINS_PER_UTXO_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetInfluenceProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.INFLUENCE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMonetaryExpandRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MONETARY_EXPAND_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetTreasuryGrowthRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.TREASURY_GROWTH_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetDecentralisationProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.DECENTRALISATION);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPriceMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.PRICE_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetPriceStepProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PRICE_STEP);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetProtocolMajorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PROTOCOL_MAJOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetProtocolMinorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PROTOCOL_MINOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetCollateralPercentProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.COLLATERAL_PERCENT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetMaxCollateralInputsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_COLLATERAL_INPUTS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEntropyProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.ENTROPY);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  // check empty
  @Test
  void testGetEmptyMinFeeAProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();

    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);

    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MIN_FEE_A);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinFeeBProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MIN_FEE_B);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBhSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BH_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }


  @Test
  void testGetEmptyKeyDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.KEY_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }


  @Test
  void testGetEmptyPoolDepositProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.POOL_DEPOSIT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxEpochProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.MAX_EPOCH);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyOptimalPoolCountProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.OPTIMAL_POOL_COUNT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinUtxoValueProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MIN_UTXO_VALUE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMinPoolCostProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MIN_POOL_COST);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxTxExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_TX_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockExMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxBlockExStepsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_BLOCK_EX_STEPS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxValSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_VAL_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyCoinsPerUtxoSizeProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.COINS_PER_UTXO_SIZE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyInfluenceProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.INFLUENCE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMonetaryExpandRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MONETARY_EXPAND_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyTreasuryGrowthRateProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.TREASURY_GROWTH_RATE);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyDecentralisationProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.DECENTRALISATION);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyPriceMemProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.PRICE_MEM);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyPriceStepProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PRICE_STEP);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyProtocolMajorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PROTOCOL_MAJOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyProtocolMinorProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.PROTOCOL_MINOR);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyCollateralPercentProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.COLLATERAL_PERCENT);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyMaxCollateralInputsProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(
        ProtocolType.MAX_COLLATERAL_INPUTS);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

  @Test
  void testGetEmptyEntropyProtocolHistory() {
    List<ParamProposal> paramProposals = new ArrayList<>();
    when(paramProposalRepository.getAllDistinctProtocolParam(any(Long.class))).thenReturn(
        paramProposals);
    Set<ProtocolHistory> response = protocolParamService.getProtocolHistory(ProtocolType.ENTROPY);
    Assertions.assertEquals(response.size(), BigInteger.ZERO.intValue());
  }

}
