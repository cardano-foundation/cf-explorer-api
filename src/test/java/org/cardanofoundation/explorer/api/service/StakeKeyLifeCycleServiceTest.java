package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.*;

import org.springframework.data.domain.*;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyLifeCycleServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.RewardType;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochParam;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
class StakeKeyLifeCycleServiceTest {

  @Mock private DelegationRepository delegationRepository;
  @Mock private StakeRegistrationRepository stakeRegistrationRepository;
  @Mock private StakeDeRegistrationRepository stakeDeRegistrationRepository;
  @Mock private StakeAddressRepository stakeAddressRepository;
  @Mock private RewardRepository rewardRepository;
  @Mock private WithdrawalRepository withdrawalRepository;
  @Mock private AddressTxAmountRepository addressTxAmountRepository;
  @Mock private TxRepository txRepository;

  @Mock FetchRewardDataService fetchRewardDataService;

  @Mock TxOutRepository txOutRepository;

  @Mock EpochParamRepository epochParamRepository;

  @InjectMocks private StakeKeyLifeCycleServiceImpl stakeKeyLifeCycleService;

  StakeAddress stakeAddress =
      StakeAddress.builder()
          .view("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
          .availableReward(BigInteger.valueOf(0))
          .build();

  @Test
  void whenStakeKeyNotFound_shouldThrowException() {
    StakeLifeCycleFilterRequest request = new StakeLifeCycleFilterRequest();
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    Pageable pageable = PageRequest.of(0, 1);
    Date fromDate = Date.from(Instant.now().minus(1, ChronoUnit.DAYS));
    Date toDate = Date.from(Instant.now());
    when(stakeAddressRepository.findByView("stake1notfound")).thenReturn(Optional.empty());
    Assertions.assertThrows(
        NoContentException.class,
        () ->
            stakeKeyLifeCycleService.getStakeRegistrations("stake1notfound", condition, pageable));
    Assertions.assertThrows(
        NoContentException.class,
        () ->
            stakeKeyLifeCycleService.getStakeDeRegistrations(
                "stake1notfound", condition, pageable));
    Assertions.assertThrows(
        BusinessException.class,
        () ->
            stakeKeyLifeCycleService.getStakeDelegationDetail(
                "stake1notfound",
                "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
    Assertions.assertThrows(
        NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegations("stake1notfound", request, pageable));
    Assertions.assertThrows(
        NoContentException.class,
        () ->
            stakeKeyLifeCycleService.getStakeRewards(
                "stake1notfound", fromDate, toDate, null, pageable));
    Assertions.assertThrows(
        NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawals("stake1notfound", request, pageable));
    Assertions.assertThrows(
        BusinessException.class,
        () ->
            stakeKeyLifeCycleService.getStakeWithdrawalDetail(
                "stake1notfound",
                "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
    Assertions.assertThrows(
        NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeWalletActivities("stake1notfound", pageable));
    Assertions.assertThrows(
        NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeRewardActivities("stake1notfound", pageable));
    Assertions.assertThrows(
        BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeLifeCycle("stake1notfound"));
  }

  @Test
  void testStakeKeyLifeCycle() {
    when(stakeAddressRepository.findByView(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna"))
        .thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.existsByAddr(stakeAddress)).thenReturn(true);
    when(stakeDeRegistrationRepository.existsByAddr(stakeAddress)).thenReturn(true);
    when(delegationRepository.existsByAddress(stakeAddress)).thenReturn(true);
    when(rewardRepository.existsByAddr(stakeAddress)).thenReturn(true);
    when(withdrawalRepository.existsByAddr(stakeAddress)).thenReturn(true);
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeLifeCycle(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna");
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.getHasRegistration());
    Assertions.assertTrue(response.getHasDeRegistration());
    Assertions.assertTrue(response.getHasDelegation());
    Assertions.assertTrue(response.getHashRewards());
    Assertions.assertTrue(response.getHasWithdrawal());
  }

  @Test
  void whenStakeAddressHaveRegistrationWithCondition_showReturnRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.getStakeRegistrationsByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    var response =
        stakeKeyLifeCycleService.getStakeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressAndTxHashHaveRegistrationAndCountEqualOne_showReturnRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueInputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(2173333L)));
    var response = stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(2000000L, response.getDeposit());
    Assertions.assertTrue(response.isJoinDepositPaid());
  }

  @Test
  void whenStakeAddressAndTxHashNotHaveRegistration_showThrowException() {
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(stakeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, txHash));
  }

  @Test
  void
      whenStakeAddressAndTxHashHaveRegistrationAndCountGreaterThanOneAndValueNegative_showReturnRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueInputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(80173333L)));
    var response = stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(2000000L, response.getDeposit());
    Assertions.assertTrue(response.isJoinDepositPaid());
  }

  @Test
  void
      whenStakeAddressAndTxHashHaveRegistrationAndCountGreaterThanOneAndValueNotNegative_showReturnRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueInputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(0L)));
    var response = stakeKeyLifeCycleService.getStakeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(2000000L, response.getDeposit());
    Assertions.assertFalse(response.isJoinDepositPaid());
  }

  @Test
  void
      whenStakeAddressAndTxHashHaveDeRegistrationAndCountEqualOne_showReturnDeRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeDeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueOutputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(1826667)));
    var response = stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(-2000000L, response.getDeposit());
    Assertions.assertTrue(response.isJoinDepositPaid());
  }

  @Test
  void whenStakeAddressAndTxHashNotHaveDeRegistration_showThrowException() {
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(stakeDeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, txHash));
  }

  @Test
  void
      whenStakeAddressAndTxHashHaveDeRegistrationAndCountGreaterThanOneAndValuePositive_showReturnDeRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeDeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueOutputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(80173333L)));
    var response = stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(-2000000L, response.getDeposit());
    Assertions.assertTrue(response.isJoinDepositPaid());
  }

  @Test
  void
      whenStakeAddressAndTxHashHaveDeRegistrationAndCountGreaterThanOneAndValueNotPositive_showReturnDeRegistrationDetail() {
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    String txHash = "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2";
    String stakeKey = "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna";
    when(projection.getTxHash()).thenReturn(txHash);
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeDeRegistrationRepository.findByAddressAndTx(stakeKey, txHash))
        .thenReturn(Optional.of(projection));
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findKeyDepositByEpochNo(any()))
        .thenReturn(BigInteger.valueOf(2000000L));
    when(txOutRepository.sumValueOutputByTxAndStakeAddress(txHash, stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(0L)));
    var response = stakeKeyLifeCycleService.getStakeDeRegistrationDetail(stakeKey, txHash);
    Assertions.assertEquals(txHash, response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getFee());
    Assertions.assertEquals(-2000000L, response.getDeposit());
    Assertions.assertFalse(response.isJoinDepositPaid());
  }

  @Test
  void whenStakeAddressHaveRegistrationWithoutCondition_showReturnRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.getStakeRegistrationsByAddress(
            any(), any(), any(), any(), any()))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    var response =
        stakeKeyLifeCycleService.getStakeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressHaveDelegationWithoutCondition_showReturnDelegations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash())
        .thenReturn("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));

    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeDelegationProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response =
        stakeKeyLifeCycleService.getStakeDelegations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveDelegationWithCondition_showReturnDelegations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash())
        .thenReturn("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeDelegationProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response =
        stakeKeyLifeCycleService.getStakeDelegations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveDelegation_showReturnDelegationDetail() {
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash())
        .thenReturn("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(projection.getPoolId())
        .thenReturn("pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu");
    when(projection.getPoolData()).thenReturn("The HIGH Pool");
    when(projection.getEpochNo()).thenReturn(369);
    when(projection.getBlockNo()).thenReturn(7895711L);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddressAndTx(any(), any()))
        .thenReturn(Optional.of(projection));
    when(addressTxAmountRepository.getBalanceByStakeAddressAndTime(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(102569063)));
    var response =
        stakeKeyLifeCycleService.getStakeDelegationDetail(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
            "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    Assertions.assertEquals(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488", response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getOutSum());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getStakeTotalAmount());
    Assertions.assertEquals(
        "pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu", response.getPoolId());
    Assertions.assertEquals("The HIGH Pool", response.getPoolName());
  }

  @Test
  void whenDelegationTxHashNotFound_shouldThrowException() {
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddressAndTx(any(), any()))
        .thenReturn(Optional.empty());
    Assertions.assertThrows(
        BusinessException.class,
        () ->
            stakeKeyLifeCycleService.getStakeDelegationDetail("stake1notfound", "txHashNotFound"));
  }

  @Test
  void whenStakeAddressHaveRewardsAvailable_showReturnRewards() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeRewardResponse rewardResponse =
        new StakeRewardResponse(
            333, Date.from(Instant.now()), BigInteger.valueOf(382916), RewardType.MEMBER);
    Date fromDate = Date.from(Instant.now().minus(1, ChronoUnit.DAYS));
    Date toDate = Date.from(Instant.now());
    Page<StakeRewardResponse> page = new PageImpl<>(List.of(rewardResponse), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(rewardRepository.findRewardByStake(
            stakeAddress,
            Timestamp.from(fromDate.toInstant()),
            Timestamp.from(toDate.toInstant()),
            null,
            pageable))
        .thenReturn(page);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeRewards(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
            fromDate,
            toDate,
            null,
            pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(333, response.getData().get(0).getEpoch());
    Assertions.assertEquals(BigInteger.valueOf(382916), response.getData().get(0).getAmount());
  }

  @Test
  void whenStakeAddressDoNotHaveRewardsAvailable_showReturnRewardNotAvailable() {
    Pageable pageable = PageRequest.of(0, 1);
    Date fromDate = Date.from(Instant.now().minus(1, ChronoUnit.DAYS));
    Date toDate = Date.from(Instant.now());
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    var response =
        stakeKeyLifeCycleService.getStakeRewards(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
            fromDate,
            toDate,
            null,
            pageable);
    Assertions.assertNull(response.getData());
  }

  @Test
  void whenStakeAddressHaveWithdrawalWithCondition_showReturnWithdrawal() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash())
        .thenReturn("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeWithdrawalProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeWithdrawals(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveWithdrawalWithoutCondition_showReturnWithdrawal() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    when(projection.getTxHash())
        .thenReturn("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeWithdrawalProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeWithdrawals(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveWithdrawal_showReturnWithdrawalDetail() {
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash())
        .thenReturn("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173157));
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(projection.getEpochNo()).thenReturn(340);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddressAndTx(any(), any()))
        .thenReturn(Optional.of(projection));
    when(addressTxAmountRepository.getBalanceByStakeAddressAndTime(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(102569063)));
    when(rewardRepository.getAvailableRewardByStakeAddressAndEpoch(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(4846486)));
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeWithdrawalDetail(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
            "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    Assertions.assertEquals(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381", response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173157), response.getFee());
    Assertions.assertEquals(BigInteger.valueOf(4846486), response.getAmount());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getStakeTotalAmount());
    Assertions.assertEquals(BigInteger.valueOf(4846486), response.getStakeRewardAvailable());
  }

  @Test
  void getStakeWithdrawalDetail_whenRewardNotAvailable_shouldReturnRewardDataNull() {
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash())
        .thenReturn("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173157));
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddressAndTx(any(), any()))
        .thenReturn(Optional.of(projection));
    when(addressTxAmountRepository.getBalanceByStakeAddressAndTime(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(102569063)));
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    var response =
        stakeKeyLifeCycleService.getStakeWithdrawalDetail(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
            "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    Assertions.assertEquals(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381", response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173157), response.getFee());
    Assertions.assertEquals(BigInteger.valueOf(4846486), response.getAmount());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getStakeTotalAmount());
    Assertions.assertNull(response.getStakeRewardAvailable());
  }

  @Test
  void whenWithdrawalTxHashNotFound_shouldThrowException() {
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddressAndTx(any(), any()))
        .thenReturn(Optional.empty());
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    Assertions.assertThrows(
        BusinessException.class,
        () ->
            stakeKeyLifeCycleService.getStakeWithdrawalDetail("stake1notfound", "txHashNotFound"));
  }

  @Test
  void whenStakeAddressHaveDeRegistrationWithCondition_showReturnDeRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(
            stakeAddress, condition.getTxHash(), fromDate, toDate, pageable))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    var response =
        stakeKeyLifeCycleService.getStakeDeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(-2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressHaveDeRegistrationWithoutCondition_showReturnDeRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");

    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(
            any(), any(), any(), any(), any()))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    var response =
        stakeKeyLifeCycleService.getStakeDeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(-2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenGetStakeWalletActivities_showReturnWalletActivities() {
    Pageable pageable = PageRequest.of(0, 6);
    StakeTxProjection projection1 = Mockito.mock(StakeTxProjection.class);
    when(projection1.getTxHash()).thenReturn("txHash1");
    when(projection1.getAmount()).thenReturn(BigInteger.valueOf(72960943));
    when(projection1.getTime()).thenReturn(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC));
    when(projection1.getValidContract()).thenReturn(true);

    StakeTxProjection projection2 = Mockito.mock(StakeTxProjection.class);
    when(projection2.getTxHash()).thenReturn("txHash2");
    when(projection2.getAmount()).thenReturn(BigInteger.valueOf(-2174301));
    when(projection2.getTime()).thenReturn(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC));
    when(projection2.getValidContract()).thenReturn(true);

    StakeTxProjection projection3 = Mockito.mock(StakeTxProjection.class);
    when(projection3.getTxHash()).thenReturn("txHash3");
    when(projection3.getAmount()).thenReturn(BigInteger.valueOf(2174301));
    when(projection3.getTime()).thenReturn(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC));
    when(projection3.getValidContract()).thenReturn(true);

    Page<StakeTxProjection> page =
        new PageImpl<>(List.of(projection1, projection2, projection3), pageable, 3);

    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(addressTxAmountRepository.findTxAndAmountByStake(stakeAddress.getView(), pageable))
        .thenReturn(page);

    var response =
        stakeKeyLifeCycleService.getStakeWalletActivities(stakeAddress.getView(), pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(3, response.getTotalItems());
    Assertions.assertEquals("txHash1", response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(72960943), response.getData().get(0).getAmount());
    Assertions.assertEquals("txHash2", response.getData().get(1).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(-2174301), response.getData().get(1).getAmount());
    Assertions.assertEquals("txHash3", response.getData().get(2).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(2174301), response.getData().get(2).getAmount());
  }

  @Test
  void getStakeRewardActivities_whenRewardAvailable_shouldReturnRewardData() {
    Pageable pageable = PageRequest.of(0, 2);
    StakeRewardResponse stakeRewardResponse =
        new StakeRewardResponse(300, new Date(), BigInteger.valueOf(1000000), RewardType.MEMBER);
    StakeRewardResponse stakeWithdrawnResponse =
        new StakeRewardResponse(301, new Date(), BigInteger.valueOf(1000000), RewardType.MEMBER);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.findEpochWithdrawalByStake(stakeAddress))
        .thenReturn(List.of(stakeWithdrawnResponse));
    when(rewardRepository.findRewardByStake(stakeAddress)).thenReturn(List.of(stakeRewardResponse));
    when(fetchRewardDataService.checkRewardAvailable(anyString())).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeRewardActivities(stakeAddress.getView(), pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(2, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(2, response.getData().size());
    Assertions.assertEquals(StakeRewardType.REWARD_WITHDRAWN, response.getData().get(0).getType());
    Assertions.assertEquals(StakeRewardType.REWARD_RECEIVED, response.getData().get(1).getType());
  }

  @Test
  void getStakeRewardActivities_whenRewardNotAvailable_shouldReturnRewardNotAvailable() {
    Pageable pageable = PageRequest.of(0, 2);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    var response =
        stakeKeyLifeCycleService.getStakeRewardActivities(stakeAddress.getView(), pageable);
    Assertions.assertNull(response.getData());
  }

  @Test
  void whenStakeAddressTxHashNull_showReturnRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.getStakeRegistrationsByAddress(
            stakeAddress, null, fromDate, toDate, pageable))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    var response =
        stakeKeyLifeCycleService.getStakeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressTxNull_showReturnDelegations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash())
        .thenReturn("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeDelegationProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response =
        stakeKeyLifeCycleService.getStakeDelegations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getData().get(0).getTxHash());
  }

  @Test
  void testGetStakeRewards_throwFetchRewardException() {
    String stakeKey = "stake_key";
    Date fromDate = new Date();
    Date toDate = new Date();
    RewardType type = RewardType.LEADER;
    Pageable pageable = PageRequest.of(0, 10);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(new StakeAddress()));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.useKoios()).thenReturn(true);

    Assertions.assertThrows(
        FetchRewardException.class,
        () -> stakeKeyLifeCycleService.getStakeRewards(stakeKey, fromDate, toDate, type, pageable));
  }

  @Test
  void whenStakeAddressTxHashNull_showReturnWithdrawal() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash())
        .thenReturn("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeWithdrawalProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    var response =
        stakeKeyLifeCycleService.getStakeWithdrawals(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getData().get(0).getTxHash());
  }

  @Test
  void testGetStakeWithdrawalDetail_throwFetchRewardException() {
    String stakeKey = "stake_key";
    String hash = "hash";

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(new StakeAddress()));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);

    Assertions.assertThrows(
        FetchRewardException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawalDetail(stakeKey, hash));
  }

  @Test
  void whenStakeAddressTxHashNull_showReturnDeRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash())
        .thenReturn("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getEpochNo()).thenReturn(200);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(
            stakeAddress, null, fromDate, toDate, pageable))
        .thenReturn(page);
    EpochParam epochParam = new EpochParam();
    epochParam.setKeyDeposit(BigInteger.valueOf(2000000L));
    epochParam.setEpochNo(200);
    when(epochParamRepository.findByEpochNoIn(any()))
        .thenReturn(Collections.singletonList(epochParam));
    var response =
        stakeKeyLifeCycleService.getStakeDeRegistrations(
            "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(-2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void testGetStakeRewardActivities_throwFetchRewardException() {
    String stakeKey = "stake_key";
    Pageable pageable = PageRequest.of(0, 10);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(new StakeAddress()));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);

    Assertions.assertThrows(
        FetchRewardException.class,
        () -> stakeKeyLifeCycleService.getStakeRewardActivities(stakeKey, pageable));
  }

  @Test
  void testGetStakeWalletActivitiesByDateRange_thenReturn() {
    String stakeKey = "stake_key";
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate =
        Timestamp.from(
            LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    Pageable pageable = PageRequest.of(0, 10);
    StakeTxProjection projection = Mockito.mock(StakeTxProjection.class);
    when(projection.getTxHash()).thenReturn("hash");
    when(projection.getAmount()).thenReturn(BigInteger.ONE);
    when(projection.getTime()).thenReturn(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC));
    when(projection.getValidContract()).thenReturn(true);
    StakeAddress stakeAddress = StakeAddress.builder().view("view").build();
    List<Long> txIds = List.of(100L);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(addressTxAmountRepository.findTxAndAmountByStakeAndDateRange(
            "view",
            fromDate.toLocalDateTime().toInstant(ZoneOffset.UTC).getEpochSecond(),
            toDate.toLocalDateTime().toInstant(ZoneOffset.UTC).getEpochSecond(),
            pageable))
        .thenReturn(new PageImpl<>(List.of(projection)));

    var response =
        stakeKeyLifeCycleService.getStakeWalletActivitiesByDateRange(stakeKey, condition, pageable);
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals("hash", response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.ONE, response.getData().get(0).getAmount());
  }
}
