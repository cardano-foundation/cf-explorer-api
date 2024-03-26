package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.*;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.test.util.ReflectionTestUtils;

import com.bloxbean.cardano.client.transaction.spec.cert.CertificateType;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeAddressStatus;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.StakeAddressMapper;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.ledgersync.*;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.RewardType;
import org.cardanofoundation.explorer.common.entity.ledgersync.*;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
public class StakeKeyServiceTest {

  @InjectMocks private StakeKeyServiceImpl stakeKeyService;
  @Mock private AddressRepository addressRepository;
  @Mock private DelegationRepository delegationRepository;
  @Mock private StakeRegistrationRepository stakeRegistrationRepository;
  @Mock private StakeDeRegistrationRepository stakeDeRegistrationRepository;
  @Mock private StakeAddressRepository stakeAddressRepository;
  @Mock private RewardRepository rewardRepository;
  @Mock private WithdrawalRepository withdrawalRepository;
  @Mock private TreasuryRepository treasuryRepository;
  @Mock private ReserveRepository reserveRepository;
  @Mock private PoolUpdateRepository poolUpdateRepository;
  @Mock private AddressTxBalanceRepository addressTxBalanceRepository;
  @Mock private StakeAddressMapper stakeAddressMapper;
  @Mock private AddressMapper addressMapper;
  @Mock private EpochRepository epochRepository;
  @Mock private RedisTemplate<String, Object> redisTemplate;
  @Mock private PoolInfoRepository poolInfoRepository;
  @Mock private FetchRewardDataService fetchRewardDataService;
  @Mock private AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;
  @Mock private ValueOperations valueOperations;
  @Mock private TxRepository txRepository;
  @Mock private StakeTxBalanceRepository stakeTxBalanceRepository;
  @Mock private YaciStakeRegistrationRepository yaciStakeRegistrationRepository;

  @Test
  void testGetDataForStakeKeyRegistration_thenReturn() {
    Pageable pageable = PageRequest.of(0, 10);
    StakeRegistration stakeRegistration = new StakeRegistration();
    stakeRegistration.setTxHash("67e20ecd3777bcdafc63e38ff830b0ab527d3bd5996d3940cefa14c61e33906c");

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getHash())
        .thenReturn("67e20ecd3777bcdafc63e38ff830b0ab527d3bd5996d3940cefa14c61e33906c");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(430);
    when(txRepository.findTxInByHashes(any())).thenReturn(List.of(tx));
    when(yaciStakeRegistrationRepository.findAllStake(CertificateType.STAKE_REGISTRATION, pageable))
        .thenReturn(new PageImpl<>(List.of(stakeRegistration)));

    StakeAddress stakeAddress = new StakeAddress();
    stakeAddress.setId(1L);
    stakeAddress.setView("stake1u9lq4sfeuzpzew7ajn7p5n8cfzxcax6jl5kljttwpeju4ec9m2tu9");
    when(stakeAddressRepository.findAllById(new HashSet<>(List.of(1L))))
        .thenReturn(List.of(stakeAddress));

    var response = stakeKeyService.getDataForStakeKeyRegistration(pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getEpoch(), 430);
    assertEquals(
        response.getData().get(0).getStakeKey(),
        "stake1u9lq4sfeuzpzew7ajn7p5n8cfzxcax6jl5kljttwpeju4ec9m2tu9");
  }

  @Test
  void testGetDataForStakeKeyDeRegistration_thenReturn() {
    Pageable pageable = PageRequest.of(0, 10);
    StakeRegistration stakeRegistration = new StakeRegistration();
    stakeRegistration.setTxHash("00020fc043ddd670fd852204d5f797a82b733a65fdb389249d80543a0fcb9723");
    stakeRegistration.setAddress(
        "stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688"); // Ensure the stake address
    // is set

    TxIOProjection tx = Mockito.mock(TxIOProjection.class);
    when(tx.getHash())
        .thenReturn("00020fc043ddd670fd852204d5f797a82b733a65fdb389249d80543a0fcb9723");
    when(tx.getTime()).thenReturn(LocalDateTime.now());
    when(tx.getEpochNo()).thenReturn(430);
    when(txRepository.findTxInByHashes(
            new HashSet<>(
                List.of("00020fc043ddd670fd852204d5f797a82b733a65fdb389249d80543a0fcb9723"))))
        .thenReturn(List.of(tx));
    when(yaciStakeRegistrationRepository.findAllStake(
            CertificateType.STAKE_DEREGISTRATION, pageable))
        .thenReturn(new PageImpl<>(List.of(stakeRegistration)));

    StakeAddress stakeAddress = new StakeAddress();
    stakeAddress.setView("stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688");
    when(stakeAddressRepository.findByViewIn(
            new HashSet<>(List.of("stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688"))))
        .thenReturn(List.of(stakeAddress));

    var response = stakeKeyService.getDataForStakeKeyDeRegistration(pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getEpoch(), 430);
    assertEquals(
        response.getData().get(0).getStakeKey(),
        "stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688");
  }

  @Test
  void testGetStakeByAddressWhenRewardAvailable_thenReturn() {
    String address =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    StakeAddress stakeAddress = StakeAddress.builder().build();
    StakeDelegationProjection sdp = Mockito.mock(StakeDelegationProjection.class);
    when(sdp.getPoolId()).thenReturn("1");
    when(sdp.getPoolData()).thenReturn("poolData");
    when(sdp.getTickerName()).thenReturn("tickerName");

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(withdrawalRepository.getRewardWithdrawnByStakeAddress(stakeKey))
        .thenReturn(Optional.of(BigInteger.ONE));
    when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey))
        .thenReturn(Optional.of(BigInteger.ONE));
    when(delegationRepository.findPoolDataByAddress(any())).thenReturn(Optional.of(sdp));
    when(stakeRegistrationRepository.findMaxTxIdByStake(any())).thenReturn(Optional.of(1L));
    when(stakeDeRegistrationRepository.findMaxTxIdByStake(any())).thenReturn(Optional.of(1L));
    when(poolUpdateRepository.findPoolByRewardAccount(any())).thenReturn(List.of("pool"));

    var response = stakeKeyService.getStakeByAddress(address);
    assertEquals(response.getStatus(), StakeAddressStatus.DEACTIVATED);
    assertEquals(response.getStakeAddress(), stakeKey);
    assertEquals(response.getTotalStake(), BigInteger.ONE);
    assertEquals(response.getRewardAvailable(), BigInteger.ZERO);
    assertEquals(response.getRewardWithdrawn(), BigInteger.ONE);
  }

  @Test
  void testGetStakeByAddressRewardNotAvailable_thenReturn() {
    String address =
        "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    StakeAddress stakeAddress = StakeAddress.builder().view(address).build();
    StakeDelegationProjection sdp = Mockito.mock(StakeDelegationProjection.class);
    when(sdp.getPoolId()).thenReturn("1");
    when(sdp.getPoolData()).thenReturn("poolData");
    when(sdp.getTickerName()).thenReturn("tickerName");

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(delegationRepository.findPoolDataByAddress(any())).thenReturn(Optional.of(sdp));
    when(yaciStakeRegistrationRepository.findMaxTxIdByStake(
            any(), CertificateType.STAKE_REGISTRATION))
        .thenReturn(Optional.of(1L));
    when(yaciStakeRegistrationRepository.findMaxTxIdByStake(
            any(), CertificateType.STAKE_DEREGISTRATION))
        .thenReturn(Optional.of(1L));
    when(poolUpdateRepository.findPoolByRewardAccount(any())).thenReturn(List.of("pool"));

    var response = stakeKeyService.getStakeByAddress(address);
    assertEquals(response.getStatus(), StakeAddressStatus.DEACTIVATED);
    assertEquals(response.getStakeAddress(), stakeKey);
    assertEquals(response.getTotalStake(), BigInteger.ONE);
    assertNull(response.getRewardAvailable());
    assertNull(response.getRewardWithdrawn());
  }

  @Test
  void testGetStake_throwException() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";

    when(stakeAddressRepository.findByView(stakeKey))
        .thenReturn(Optional.of(StakeAddress.builder().build()));
    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);

    assertThrows(FetchRewardException.class, () -> stakeKeyService.getStake(stakeKey));
  }

  @Test
  void testGetDelegationHistories_thenReturn() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);
    StakeDelegationProjection sdp = Mockito.mock(StakeDelegationProjection.class);
    when(sdp.getPoolId()).thenReturn("1");
    when(sdp.getPoolData()).thenReturn("poolData");
    when(sdp.getTickerName()).thenReturn("tickerName");

    when(delegationRepository.findDelegationByAddress(stakeKey, pageable))
        .thenReturn(new PageImpl<>(List.of(sdp)));

    var response = stakeKeyService.getDelegationHistories(stakeKey, pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getPoolId(), "1");
    assertEquals(response.getData().get(0).getPoolData(), "poolData");
    assertEquals(response.getData().get(0).getTickerName(), "tickerName");
  }

  @Test
  void testGetStakeHistories_thenReturn() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);
    StakeAddress stakeAddress =
        StakeAddress.builder()
            .view("stake_test1uzzzmcnyjfqle0qgal8234yvvp9q0u9e9qdxhzrmq5el7mc25kaav")
            .build();
    StakeHistoryProjection shp1 = Mockito.mock(StakeHistoryProjection.class);
    when(shp1.getBlockNo()).thenReturn(1L);
    when(shp1.getBlockIndex()).thenReturn(1);
    StakeHistoryProjection shp2 = Mockito.mock(StakeHistoryProjection.class);
    when(shp2.getBlockNo()).thenReturn(2L);
    when(shp2.getBlockIndex()).thenReturn(2);
    List<StakeHistoryProjection> list1 = new ArrayList<>();
    list1.add(shp1);
    List<StakeHistoryProjection> list2 = new ArrayList<>();
    list2.add(shp2);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(yaciStakeRegistrationRepository.getStakeRegistrationsByAddress(
            stakeAddress.getView(), CertificateType.STAKE_REGISTRATION))
        .thenReturn(list1);
    when(yaciStakeRegistrationRepository.getStakeRegistrationsByAddress(
            stakeAddress.getView(), CertificateType.STAKE_DEREGISTRATION))
        .thenReturn(list2);

    var response = stakeKeyService.getStakeHistories(stakeKey, pageable);
    assertEquals(response.getTotalItems(), 2);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getBlockNo(), 2L);
    assertEquals(response.getData().get(0).getBlockIndex(), 2);
    assertEquals(response.getData().get(1).getBlockNo(), 1L);
    assertEquals(response.getData().get(1).getBlockIndex(), 1);
  }

  @Test
  void testGetStakeHistories_throwException() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.empty());

    assertThrows(
        NoContentException.class, () -> stakeKeyService.getStakeHistories(stakeKey, pageable));
  }

  @Test
  void testGetWithdrawalHistories_thenReturn() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);
    StakeWithdrawalProjection swp = Mockito.mock(StakeWithdrawalProjection.class);
    when(swp.getAmount()).thenReturn(BigInteger.ONE);
    when(swp.getEpochNo()).thenReturn(400);

    when(withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable))
        .thenReturn(new PageImpl<>(List.of(swp)));

    var response = stakeKeyService.getWithdrawalHistories(stakeKey, pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getAmount(), BigInteger.ONE);
    assertEquals(response.getData().get(0).getEpochNo(), 400);
  }

  @Test
  void testGetInstantaneousRewards_thenReturn() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);
    StakeInstantaneousRewardsProjection sirp1 =
        Mockito.mock(StakeInstantaneousRewardsProjection.class);
    when(sirp1.getBlockNo()).thenReturn(1L);
    when(sirp1.getBlockIndex()).thenReturn(1);
    StakeInstantaneousRewardsProjection sirp2 =
        Mockito.mock(StakeInstantaneousRewardsProjection.class);
    when(sirp2.getBlockNo()).thenReturn(2L);
    when(sirp2.getBlockIndex()).thenReturn(2);
    List<StakeInstantaneousRewardsProjection> list1 = new ArrayList<>();
    List<StakeInstantaneousRewardsProjection> list2 = new ArrayList<>();
    list1.add(sirp1);
    list2.add(sirp2);

    when(treasuryRepository.getTreasuryByAddress(stakeKey)).thenReturn(list1);
    when(reserveRepository.getReserveByAddress(stakeKey)).thenReturn(list2);

    var response = stakeKeyService.getInstantaneousRewards(stakeKey, pageable);
    assertEquals(response.getTotalItems(), 2);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(1).getBlockNo(), 1L);
    assertEquals(response.getData().get(1).getBlockIndex(), 1);
    assertEquals(response.getData().get(0).getBlockNo(), 2L);
    assertEquals(response.getData().get(0).getBlockIndex(), 2);
  }

  @Test
  void getTopDelegators_whenRewardDataAvailable_shouldReturnRewardData() {
    Pageable pageable = PageRequest.of(0, 10);
    StakeAddressProjection sap = Mockito.mock(StakeAddressProjection.class);
    when(sap.getStakeAddress()).thenReturn("address");
    when(sap.getId()).thenReturn(1L);
    when(sap.getTotalStake()).thenReturn(BigInteger.ONE);
    StakeWithdrawalProjection swp = Mockito.mock(StakeWithdrawalProjection.class);
    when(swp.getStakeAddressId()).thenReturn(1L);
    when(swp.getAmount()).thenReturn(BigInteger.ONE);
    StakeRewardProjection srp = Mockito.mock(StakeRewardProjection.class);
    when(srp.getStakeAddressId()).thenReturn(1L);
    when(srp.getAmount()).thenReturn(BigInteger.ONE);
    StakeDelegationProjection sdp = Mockito.mock(StakeDelegationProjection.class);
    when(sdp.getStakeAddress()).thenReturn("address");

    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(stakeAddressRepository.findStakeAddressOrderByBalance(pageable)).thenReturn(List.of(sap));
    when(fetchRewardDataService.checkRewardAvailable(List.of("address"))).thenReturn(true);
    when(withdrawalRepository.getRewardWithdrawnByAddrIn(any())).thenReturn(List.of(swp));
    when(rewardRepository.getTotalRewardByStakeAddressIn(any())).thenReturn(List.of(srp));
    when(delegationRepository.findPoolDataByAddressIn(Set.of("address"))).thenReturn(List.of(sdp));
    when(stakeAddressMapper.fromStakeAddressAndDelegationProjection(any(), any()))
        .thenReturn(StakeFilterResponse.builder().build());

    var response = stakeKeyService.getTopDelegators(pageable);
    assertEquals(response.getTotalItems(), 10);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getBalance(), BigInteger.ONE);
  }

  @Test
  void getTopDelegators_whenRewardDataNotAvailable_shouldReturnEmptyRewardData() {
    Pageable pageable = PageRequest.of(0, 10);
    StakeAddressProjection sap = Mockito.mock(StakeAddressProjection.class);

    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(stakeAddressRepository.findStakeAddressOrderByBalance(pageable)).thenReturn(List.of(sap));

    var response = stakeKeyService.getTopDelegators(pageable);
    assertNull(response.getData());
  }

  @Test
  void testGetTopDelegators_thenReturnKoios() {
    Pageable pageable = PageRequest.of(0, 10);
    StakeAddressProjection sap = Mockito.mock(StakeAddressProjection.class);
    when(sap.getStakeAddress()).thenReturn("address");

    when(stakeAddressRepository.findStakeAddressOrderByBalance(pageable)).thenReturn(List.of(sap));
    when(fetchRewardDataService.checkRewardAvailable(List.of("address"))).thenReturn(false);
    when(fetchRewardDataService.fetchReward(List.of("address"))).thenReturn(false);
    when(fetchRewardDataService.useKoios()).thenReturn(true);

    assertThrows(FetchRewardException.class, () -> stakeKeyService.getTopDelegators(pageable));
  }

  @Test
  void testGetAddresses_thenReturn() {
    String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
    Pageable pageable = PageRequest.of(0, 10);
    Address address = Address.builder().address("address").balance(BigInteger.ONE).build();

    when(addressRepository.findByStakeAddress(stakeKey, pageable))
        .thenReturn(new PageImpl<>(List.of(address)));
    when(addressMapper.fromAddressToFilterResponse(address))
        .thenReturn(
            AddressFilterResponse.builder().address("address").balance(BigInteger.ONE).build());

    var response = stakeKeyService.getAddresses(stakeKey, pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getAddress(), "address");
    assertEquals(response.getData().get(0).getBalance(), BigInteger.ONE);
  }

  @Test
  void testGetStakeAnalytics_thenReturnRewardDataNull() {
    ReflectionTestUtils.setField(stakeKeyService, "network", "mainnet");
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));
    when(fetchRewardDataService.useKoios()).thenReturn(false);

    var response = stakeKeyService.getStakeAnalytics();
    assertEquals(response.getActiveStake(), null);
    assertEquals(response.getLiveStake(), null);
  }

  @Test
  void testGetStakeAnalytics_thenReturnKoios() {
    ReflectionTestUtils.setField(stakeKeyService, "network", "mainnet");
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(poolInfoRepository.getTotalActiveStake(400)).thenReturn(BigInteger.ONE);
    when(poolInfoRepository.getTotalLiveStake(400)).thenReturn(BigInteger.TWO);

    var response = stakeKeyService.getStakeAnalytics();
    assertEquals(response.getActiveStake(), BigInteger.ONE);
    assertEquals(response.getLiveStake(), BigInteger.TWO);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnV1() {
    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_DAY;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection min = Mockito.mock(MinMaxProjection.class);
    when(min.getMinVal()).thenReturn(BigInteger.ONE);
    when(min.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(aggregateAddressTxBalanceRepository.sumBalanceByStakeAddressId(any(), any()))
        .thenReturn(Optional.of(BigInteger.ONE));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(min);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnV2() {

    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_WEEK;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ONE);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(aggregateAddressTxBalanceRepository.sumBalanceByStakeAddressId(any(), any()))
        .thenReturn(Optional.of(BigInteger.ONE));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnV3() {
    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_DAY;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ONE);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any(), any()))
        .thenReturn(Optional.of(BigInteger.ZERO));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnV4() {
    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_WEEK;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ONE);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnBalanceZero() {
    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_WEEK;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ONE);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeBalanceAnalytics_thenReturnBalanceZeroV2() {
    String stakeKey = "stake_key";
    AnalyticType type = AnalyticType.ONE_DAY;
    StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
    MinMaxProjection minMaxProjection = Mockito.mock(MinMaxProjection.class);
    when(minMaxProjection.getMinVal()).thenReturn(BigInteger.ONE);
    when(minMaxProjection.getMaxVal()).thenReturn(BigInteger.TEN);

    when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
    when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any(), any()))
        .thenReturn(Optional.of(BigInteger.ZERO));
    when(stakeTxBalanceRepository.findMinMaxBalanceByStakeAddress(any(), any(), any(), any()))
        .thenReturn(minMaxProjection);

    var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
    assertNotNull(response);
  }

  @Test
  void testGetStakeRewardAnalytics_throwException() {
    String stakeKey = "stake_key";

    when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);
    when(fetchRewardDataService.useKoios()).thenReturn(true);

    assertThrows(
        FetchRewardException.class, () -> stakeKeyService.getStakeRewardAnalytics(stakeKey));
  }

  @Test
  void getStakeRewardAnalytics_whenRewardNotAvailable_shouldReturnNull() {
    String stakeKey = "stake_key";

    when(fetchRewardDataService.useKoios()).thenReturn(false);
    var response = stakeKeyService.getStakeRewardAnalytics(stakeKey);
    assertNull(response);
  }

  @Test
  void testGetStakeByAddress_throwException() {
    String address = "wrong_address";

    assertThrows(BusinessException.class, () -> stakeKeyService.getStakeByAddress(address));
  }

  @Test
  void getStakeAddressRewardDistributionInfo_whenRewardDataAvailable_shouldReturnRewardData() {
    String stakeKey = "stake_key";

    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(10)));
    when(rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey))
        .thenReturn(Set.of(RewardType.MEMBER));

    var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
    assertEquals(stakeKey, response.getStakeAddress());
    assertEquals(BigInteger.valueOf(10), response.getRewardAvailable());
    assertTrue(response.isHasMemberReward());
    assertFalse(response.isHasLeaderReward());
  }

  @Test
  void
      getStakeAddressRewardDistributionInfo_whenRewardDataNotAvailable_shouldNotReturnRewardData() {
    String stakeKey = "stake_key";

    when(fetchRewardDataService.useKoios()).thenReturn(false);
    var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
    assertEquals(stakeKey, response.getStakeAddress());
    assertNull(response.getRewardAvailable());
  }

  @Test
  void testRewardDistribution_thenReturnNoType() {
    String stakeKey = "stake_key";

    when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey))
        .thenReturn(Optional.of(BigInteger.valueOf(10)));
    when(rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey)).thenReturn(new HashSet<>());

    var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
    assertEquals(stakeKey, response.getStakeAddress());
    assertEquals(BigInteger.valueOf(10), response.getRewardAvailable());
    assertFalse(response.isHasMemberReward());
    assertFalse(response.isHasLeaderReward());
  }
}
