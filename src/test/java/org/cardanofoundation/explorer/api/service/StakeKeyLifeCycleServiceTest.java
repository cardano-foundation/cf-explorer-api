package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.time.temporal.ChronoUnit;
import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeTxType;
import org.cardanofoundation.explorer.api.model.request.stake.StakeLifeCycleFilterRequest;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.projection.StakeDelegationProjection;
import org.cardanofoundation.explorer.api.projection.StakeHistoryProjection;
import org.cardanofoundation.explorer.api.projection.StakeTxProjection;
import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.RewardRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.repository.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyLifeCycleServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

@ExtendWith(MockitoExtension.class)
class StakeKeyLifeCycleServiceTest {

  @Mock
  private DelegationRepository delegationRepository;
  @Mock
  private StakeRegistrationRepository stakeRegistrationRepository;
  @Mock
  private StakeDeRegistrationRepository stakeDeRegistrationRepository;
  @Mock
  private StakeAddressRepository stakeAddressRepository;
  @Mock
  private RewardRepository rewardRepository;
  @Mock
  private WithdrawalRepository withdrawalRepository;
  @Mock
  private AddressTxBalanceRepository addressTxBalanceRepository;

  @Mock
  private TxRepository txRepository;

  @Mock FetchRewardDataService fetchRewardDataService;

  @InjectMocks
  private StakeKeyLifeCycleServiceImpl stakeKeyLifeCycleService;

  StakeAddress stakeAddress = StakeAddress.builder()
      .view("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
      .balance(BigInteger.valueOf(1000000000))
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
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeRegistrations("stake1notfound", condition,
            pageable));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeDeRegistrations("stake1notfound", condition,
            pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegationDetail("stake1notfound",
            "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegations("stake1notfound", request, pageable));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeRewards("stake1notfound", fromDate, toDate, null , pageable));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawals("stake1notfound", request, pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawalDetail("stake1notfound",
            "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeWalletActivities("stake1notfound", pageable));
    Assertions.assertThrows(NoContentException.class,
        () -> stakeKeyLifeCycleService.getStakeRewardActivities("stake1notfound", pageable));
  }

  @Test
  void whenStakeAddressHaveRegistrationWithCondition_showReturnRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash()).thenReturn(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getDeposit()).thenReturn(2000000L);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeAddress,
        condition.getTxHash(),
        fromDate, toDate, pageable)).thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeRegistrations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressHaveRegistrationWithoutCondition_showReturnRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash()).thenReturn(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getDeposit()).thenReturn(2000000L);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeRegistrationRepository.getStakeRegistrationsByAddress(any(), any(),
        any(), any(), any())).thenReturn(page);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    var response = stakeKeyLifeCycleService.getStakeRegistrations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressHaveDelegationWithoutCondition_showReturnDelegations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));

    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeDelegationProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeDelegations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveDelegationWithCondition_showReturnDelegations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeDelegationProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeDelegations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveDelegation_showReturnDelegationDetail() {
    StakeDelegationProjection projection = Mockito.mock(StakeDelegationProjection.class);
    when(projection.getTxHash()).thenReturn(
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    when(projection.getOutSum()).thenReturn(BigInteger.valueOf(102569063));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(projection.getPoolId()).thenReturn(
        "pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu");
    when(projection.getPoolData()).thenReturn(
        "The HIGH Pool");
    when(projection.getEpochNo()).thenReturn(369);
    when(projection.getBlockNo()).thenReturn(7895711L);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddressAndTx(any(), any()))
        .thenReturn(Optional.of(projection));
    when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(102569063)));
    var response = stakeKeyLifeCycleService.getStakeDelegationDetail(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
        "bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    Assertions.assertEquals("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488",
        response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getOutSum());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getStakeTotalAmount());
    Assertions.assertEquals("pool1tay8z4sq4a4gmyhnygyt0t5j84z8epwjra06wq28jnnmschkkuu",
        response.getPoolId());
    Assertions.assertEquals("The HIGH Pool", response.getPoolName());

  }

  @Test
  void whenDelegationTxHashNotFound_shouldThrowException() {
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(delegationRepository.findDelegationByAddressAndTx(any(), any()))
        .thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegationDetail("stake1notfound",
            "txHashNotFound"));
  }

  @Test
  void whenStakeAddressHaveRewards_showReturnRewards() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeRewardResponse rewardResponse = new StakeRewardResponse(333,
        Date.from(Instant.now()), BigInteger.valueOf(382916));
    Date fromDate = Date.from(Instant.now().minus(1, ChronoUnit.DAYS));
    Date toDate = Date.from(Instant.now());
    Page<StakeRewardResponse> page = new PageImpl<>(List.of(rewardResponse), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(rewardRepository.findRewardByStake(stakeAddress, Timestamp.from(fromDate.toInstant()),
        Timestamp.from(toDate.toInstant()), null, pageable)).thenReturn(page);
    when(fetchRewardDataService.checkRewardAvailable(any())).thenReturn(true);
    var response = stakeKeyLifeCycleService.getStakeRewards(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", fromDate, toDate,null,  pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals(333, response.getData().get(0).getEpoch());
    Assertions.assertEquals(BigInteger.valueOf(382916), response.getData().get(0).getAmount());
  }

  @Test
  void whenStakeAddressHaveWithdrawalWithCondition_showReturnWithdrawal() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash()).thenReturn(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeWithdrawalProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeWithdrawals(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getData().get(0).getTxHash());
  }

  @Test
  void whenStakeAddressHaveWithdrawalWithoutCondition_showReturnWithdrawal() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    when(projection.getTxHash()).thenReturn(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeWithdrawalProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddress(any(), any(), any(), any(), any()))
        .thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeWithdrawals(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getData().get(0).getTxHash());
  }


  @Test
  void whenStakeAddressHaveWithdrawal_showReturnWithdrawalDetail() {
    StakeWithdrawalProjection projection = Mockito.mock(StakeWithdrawalProjection.class);
    when(projection.getTxHash()).thenReturn(
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173157));
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(4846486));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    when(projection.getEpochNo()).thenReturn(340);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddressAndTx(any(), any()))
        .thenReturn(Optional.of(projection));
    when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(102569063)));
    when(rewardRepository.getAvailableRewardByStakeAddressAndEpoch(any(), any()))
        .thenReturn(Optional.of(BigInteger.valueOf(4846486)));
    when(fetchRewardDataService.checkRewardAvailable(any())).thenReturn(true);
    var response = stakeKeyLifeCycleService.getStakeWithdrawalDetail(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna",
        "91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381");
    Assertions.assertEquals("91d4995345d7aa62f74167d22f596dbd10f486785be3605b0d3bc0ec1bd9c381",
        response.getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173157), response.getFee());
    Assertions.assertEquals(BigInteger.valueOf(4846486), response.getAmount());
    Assertions.assertEquals(BigInteger.valueOf(102569063), response.getStakeTotalAmount());
    Assertions.assertEquals(BigInteger.valueOf(4846486), response.getStakeRewardAvailable());

  }

  @Test
  void whenWithdrawalTxHashNotFound_shouldThrowException() {
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.getWithdrawalByAddressAndTx(any(), any()))
        .thenReturn(Optional.empty());
    when(fetchRewardDataService.checkRewardAvailable(any())).thenReturn(true);
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawalDetail("stake1notfound",
            "txHashNotFound"));
  }

  @Test
  void whenStakeAddressHaveDeRegistrationWithCondition_showReturnDeRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    Timestamp fromDate = Timestamp.valueOf("1970-01-01 00:00:00");
    Timestamp toDate = Timestamp.from(LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)
        .toInstant(ZoneOffset.UTC));
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");
    condition.setFromDate(fromDate);
    condition.setToDate(toDate);
    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash()).thenReturn(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getDeposit()).thenReturn(2000000L);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeAddress,
        condition.getTxHash(), fromDate, toDate, pageable)).thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeDeRegistrations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenStakeAddressHaveDeRegistrationWithoutCondition_showReturnDeRegistrations() {
    Pageable pageable = PageRequest.of(0, 1);
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    condition.setTxHash("bd80f5d56419eed99b45b45c58468213be28584ce64fcd2b6bd1300af8b6e488");

    StakeHistoryProjection projection = Mockito.mock(StakeHistoryProjection.class);
    when(projection.getTxHash()).thenReturn(
        "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2");
    when(projection.getFee()).thenReturn(BigInteger.valueOf(173333));
    when(projection.getDeposit()).thenReturn(2000000L);
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(any(), any(), any(), any(),
        any())).thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeDeRegistrations(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", condition, pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2",
        response.getData().get(0).getTxHash());
    Assertions.assertEquals(BigInteger.valueOf(173333), response.getData().get(0).getFee());
    Assertions.assertEquals(2000000L, response.getData().get(0).getDeposit());
  }

  @Test
  void whenGetStakeWalletActivities_showReturnWalletActivities() {
    Pageable pageable = PageRequest.of(0, 6);
    StakeTxProjection projection = Mockito.mock(StakeTxProjection.class);
    when(projection.getTxId()).thenReturn(100L);
    when(projection.getAmount()).thenReturn(BigInteger.valueOf(-500174301));
    when(projection.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    StakeTxProjection projection1 = Mockito.mock(StakeTxProjection.class);
    when(projection1.getTxId()).thenReturn(101L);
    when(projection1.getAmount()).thenReturn(BigInteger.valueOf(72960943));
    when(projection1.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    StakeTxProjection projection2 = Mockito.mock(StakeTxProjection.class);
    when(projection2.getTxId()).thenReturn(102L);
    when(projection2.getAmount()).thenReturn(BigInteger.valueOf(-2174301));
    when(projection2.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    StakeTxProjection projection3 = Mockito.mock(StakeTxProjection.class);
    when(projection3.getTxId()).thenReturn(103L);
    when(projection3.getAmount()).thenReturn(BigInteger.valueOf(-181385));
    when(projection3.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    StakeTxProjection projection4 = Mockito.mock(StakeTxProjection.class);
    when(projection4.getTxId()).thenReturn(104L);
    when(projection4.getAmount()).thenReturn(BigInteger.valueOf(-172761));
    when(projection4.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    StakeTxProjection projection5 = Mockito.mock(StakeTxProjection.class);
    when(projection5.getTxId()).thenReturn(105L);
    when(projection5.getAmount()).thenReturn(BigInteger.valueOf(-2174301));
    when(projection5.getTime()).thenReturn(Timestamp.valueOf(LocalDateTime.now()));
    Page<StakeTxProjection> page = new PageImpl<>(
        List.of(projection, projection1, projection2, projection3, projection4, projection5), pageable, 6);
    List<Tx> txList = new ArrayList<>();
    txList.add(Tx.builder()
        .id(100L).hash("11ae03377b31c749d2d549674100986ec4ee68ac72e211404647f5ae0ce8686b")
        .fee(BigInteger.valueOf(174301))
        .deposit(0L)
        .validContract(true)
        .build());
    txList.add(Tx.builder()
        .id(101L).hash("3a4de98d4652281ff2c747bbe0582c985d590ca57bc783fa3e5e0c23b126d6ca")
        .fee(BigInteger.valueOf(175093))
        .deposit(0L)
        .validContract(true)
        .build());
    txList.add(Tx.builder()
        .id(102L).hash("17c5b738f4de8a67882791d261f7fcbd6671e4eae29936171ac48307c18d191e")
        .fee(BigInteger.valueOf(174301))
        .validContract(true)
        .deposit(2000000L)
        .build());
    txList.add(Tx.builder()
        .id(103L).hash("5b995ad32ba2c0bb86e224441845d8adc71a03be932360b93e1a04bd459b02da")
        .fee(BigInteger.valueOf(-181385))
        .deposit(0L)
        .validContract(false)
        .build());
    txList.add(Tx.builder()
        .id(104L).hash("e985489b135b68add6f0f13a3e3b7f513f9e56e4710faee8b0c5065afb4419d1")
        .fee(BigInteger.valueOf(172761))
        .deposit(0L)
        .validContract(false)
        .build());
    txList.add(Tx.builder()
            .id(105L).hash("817c26fc41a840f640c83ddda096a51406649402fc7dde0739131b209e9432b6")
            .fee(BigInteger.valueOf(24027))
            .deposit(-2000000L)
            .validContract(false)
            .build());
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(addressTxBalanceRepository.findTxAndAmountByStake(stakeAddress.getView(),
        pageable)).thenReturn(page);
    when(txRepository.findByIdIn(any())).thenReturn(txList);
    when(stakeRegistrationRepository.getStakeRegistrationsByAddressAndTxIn(any(), any()))
        .thenReturn(List.of(102L));
    when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddressAndTxIn(any(), any()))
        .thenReturn(List.of(105L));
    when(delegationRepository.findDelegationByAddressAndTxIn(any(), any()))
        .thenReturn(List.of(104L));
    var response = stakeKeyLifeCycleService.getStakeWalletActivities(stakeAddress.getView(),
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(6, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(6, response.getData().size());
    Assertions.assertEquals(StakeTxType.SENT, response.getData().get(0).getType());
    Assertions.assertEquals(StakeTxType.RECEIVED, response.getData().get(1).getType());
    Assertions.assertEquals(StakeTxType.CERTIFICATE_HOLD_PAID,response.getData().get(2).getType());
    Assertions.assertEquals(StakeTxType.FEE_PAID, response.getData().get(3).getType());
    Assertions.assertEquals(StakeTxType.CERTIFICATE_FEE_PAID, response.getData().get(4).getType());
    Assertions.assertEquals(StakeTxType.CERTIFICATE_HOLD_DEPOSIT_REFUNDED, response.getData().get(5).getType());
  }

  @Test
  void whenGetStakeRewardActivities_showReturnRewardActivities() {
    Pageable pageable = PageRequest.of(0, 2);
    StakeRewardResponse stakeRewardResponse = new StakeRewardResponse(300, new Date(),
        BigInteger.valueOf(1000000));
    StakeRewardResponse stakeWithdrawnResponse = new StakeRewardResponse(301, new Date(),
        BigInteger.valueOf(1000000));
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(withdrawalRepository.findEpochWithdrawalByStake(stakeAddress)).thenReturn(
        List.of(stakeWithdrawnResponse));
    when(rewardRepository.findRewardByStake(stakeAddress)).thenReturn(
        List.of(stakeRewardResponse));
    when(fetchRewardDataService.checkRewardAvailable(any())).thenReturn(true);
    var response = stakeKeyLifeCycleService.getStakeRewardActivities(stakeAddress.getView(),
        pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(2, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(2, response.getData().size());
    Assertions.assertEquals(StakeRewardType.REWARD_WITHDRAWN, response.getData().get(0).getType());
    Assertions.assertEquals(StakeRewardType.REWARD_RECEIVED, response.getData().get(1).getType());
  }

}
