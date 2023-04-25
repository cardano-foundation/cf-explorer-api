package com.cardano.explorer.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.cardano.explorer.model.request.stake.StakeLifeCycleFilterRequest;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.projection.StakeDelegationProjection;
import com.cardano.explorer.projection.StakeHistoryProjection;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.RewardRepository;
import com.cardano.explorer.repository.StakeAddressRepository;
import com.cardano.explorer.repository.StakeDeRegistrationRepository;
import com.cardano.explorer.repository.StakeRegistrationRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.impl.StakeKeyLifeCycleServiceImpl;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
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

  @InjectMocks
  private StakeKeyLifeCycleServiceImpl stakeKeyLifeCycleService;

  StakeAddress stakeAddress = StakeAddress.builder()
      .view("stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna")
      .balance(BigInteger.valueOf(1000000000))
      .availableReward(BigInteger.valueOf(0))
      .isDeleted(false)
      .build();

  @Test
  void whenStakeKeyNotFound_shouldThrowException() {
    StakeLifeCycleFilterRequest request = new StakeLifeCycleFilterRequest();
    StakeLifeCycleFilterRequest condition = new StakeLifeCycleFilterRequest();
    Pageable pageable = PageRequest.of(0, 1);
    when(stakeAddressRepository.findByView("stake1notfound")).thenReturn(Optional.empty());
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeRegistrations("stake1notfound", condition,
            pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDeRegistrations("stake1notfound", condition,
            pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegationDetail("stake1notfound",
            "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeDelegations("stake1notfound", request, pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeRewards("stake1notfound", pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawals("stake1notfound", request, pageable));
    Assertions.assertThrows(BusinessException.class,
        () -> stakeKeyLifeCycleService.getStakeWithdrawalDetail("stake1notfound",
            "f8680884f04ef2b10fdc778e2aa981b909f7268570db231a1d0baac377620ea2"));
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
        "{\"name\":\"The HIGH Pool\",\"description\":\"Reliable Efficient Fast - Operated in Germany with german precision\",\"ticker\":\"HIGH\",\"homepage\":\"https://notyetdefined.com\"}");
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
    Page<StakeRewardResponse> page = new PageImpl<>(List.of(rewardResponse), pageable, 1);
    when(stakeAddressRepository.findByView(anyString())).thenReturn(Optional.of(stakeAddress));
    when(rewardRepository.findRewardByStake(stakeAddress, pageable)).thenReturn(page);
    var response = stakeKeyLifeCycleService.getStakeRewards(
        "stake1u98ujxfgzdm8yh6qsaar54nmmr50484t4ytphxjex3zxh7g4tuwna", pageable);
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


}
