package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.StakeAddressStatus;
import org.cardanofoundation.explorer.api.exception.FetchRewardException;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.StakeAddressMapper;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.StakeFilterResponse;
import org.cardanofoundation.explorer.api.projection.*;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.impl.StakeKeyServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class StakeKeyServiceTest {

    @InjectMocks
    private StakeKeyServiceImpl stakeKeyService;
    @Mock
    private AddressRepository addressRepository;
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
    private TreasuryRepository treasuryRepository;
    @Mock
    private ReserveRepository reserveRepository;
    @Mock
    private PoolUpdateRepository poolUpdateRepository;
    @Mock
    private AddressTxBalanceRepository addressTxBalanceRepository;
    @Mock
    private StakeAddressMapper stakeAddressMapper;
    @Mock
    private AddressMapper addressMapper;
    @Mock
    private EpochRepository epochRepository;
    @Mock
    private RedisTemplate<String, Object> redisTemplate;
    @Mock
    private PoolInfoRepository poolInfoRepository;
    @Mock
    private FetchRewardDataService fetchRewardDataService;
    @Mock
    private AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;
    @Mock
    private ValueOperations valueOperations;
    @Mock
    private TxRepository txRepository;

    @Test
    void testGetDataForStakeKeyRegistration_thenReturn() {
        Pageable pageable = PageRequest.of(0, 10);
        StakeRegistration stakeRegistration = new StakeRegistration();
        stakeRegistration.setStakeAddressId(1L);
        stakeRegistration.setTxId(1L);

        TxIOProjection tx = Mockito.mock(TxIOProjection.class);
        when(tx.getId()).thenReturn(1L);
        when(tx.getHash()).thenReturn("67e20ecd3777bcdafc63e38ff830b0ab527d3bd5996d3940cefa14c61e33906c");
        when(tx.getTime()).thenReturn(LocalDateTime.now());
        when(tx.getEpochNo()).thenReturn(430);
        when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));
        when(stakeRegistrationRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(stakeRegistration)));

        StakeAddress stakeAddress = new StakeAddress();
        stakeAddress.setId(1L);
        stakeAddress.setView("stake1u9lq4sfeuzpzew7ajn7p5n8cfzxcax6jl5kljttwpeju4ec9m2tu9");
        when(stakeAddressRepository.findAllById(new HashSet<>(List.of(1L)))).thenReturn(List.of(stakeAddress));

        var response = stakeKeyService.getDataForStakeKeyRegistration(pageable);
        assertEquals(response.getTotalItems(), 1);
        assertEquals(response.getTotalPages(), 1);
        assertEquals(response.getCurrentPage(), 0);
        assertEquals(response.getData().get(0).getEpoch(), 430);
        assertEquals(response.getData().get(0).getStakeKey(),
            "stake1u9lq4sfeuzpzew7ajn7p5n8cfzxcax6jl5kljttwpeju4ec9m2tu9");
    }

    @Test
    void testGetDataForStakeKeyDeRegistration_thenReturn() {
        Pageable pageable = PageRequest.of(0, 10);
        StakeDeregistration stakeRegistration = new StakeDeregistration();
        stakeRegistration.setStakeAddressId(1L);
        stakeRegistration.setTxId(1L);

        TxIOProjection tx = Mockito.mock(TxIOProjection.class);
        when(tx.getId()).thenReturn(1L);
        when(tx.getHash()).thenReturn("c7d41594ea5fa56b9daf6dbbddeb61f74b1dd06392ac12830920de26d2f5f93d");
        when(tx.getTime()).thenReturn(LocalDateTime.now());
        when(tx.getEpochNo()).thenReturn(430);
        when(txRepository.findTxIn(new HashSet<>(List.of(1L)))).thenReturn(List.of(tx));
        when(stakeDeRegistrationRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(stakeRegistration)));

        StakeAddress stakeAddress = new StakeAddress();
        stakeAddress.setId(1L);
        stakeAddress.setView("stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688");
        when(stakeAddressRepository.findAllById(new HashSet<>(List.of(1L)))).thenReturn(List.of(stakeAddress));

        var response = stakeKeyService.getDataForStakeKeyDeRegistration(pageable);
        assertEquals(response.getTotalItems(), 1);
        assertEquals(response.getTotalPages(), 1);
        assertEquals(response.getCurrentPage(), 0);
        assertEquals(response.getData().get(0).getEpoch(), 430);
        assertEquals(response.getData().get(0).getStakeKey(),
            "stake1u9q7f0kyqfe2ljlfwzyad9hl9n0kqg3zsq35kq8jxrnce4q4wy688");
    }

    @Test
    void testGetStakeByAddress_thenReturn() {
        String address = "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm";
        String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
        StakeAddress stakeAddress = StakeAddress.builder().balance(BigInteger.ONE).build();
        StakeDelegationProjection sdp = Mockito.mock(StakeDelegationProjection.class);
        when(sdp.getPoolId()).thenReturn("1");
        when(sdp.getPoolData()).thenReturn("poolData");
        when(sdp.getTickerName()).thenReturn("tickerName");

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(true);
        when(withdrawalRepository.getRewardWithdrawnByStakeAddress(stakeKey)).thenReturn(Optional.of(BigInteger.ONE));
        when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey)).thenReturn(Optional.of(BigInteger.ONE));
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
    void testGetStake_throwException() {
        String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(StakeAddress.builder().balance(BigInteger.ONE).build()));
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

        when(delegationRepository.findDelegationByAddress(stakeKey, pageable)).thenReturn(new PageImpl<>(List.of(sdp)));

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
        StakeAddress stakeAddress = StakeAddress.builder().balance(BigInteger.ONE).build();
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
        when(stakeRegistrationRepository.getStakeRegistrationsByAddress(stakeAddress)).thenReturn(list1);
        when(stakeDeRegistrationRepository.getStakeDeRegistrationsByAddress(stakeAddress)).thenReturn(list2);

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

        assertThrows(NoContentException.class, () -> stakeKeyService.getStakeHistories(stakeKey, pageable));
    }

    @Test
    void testGetWithdrawalHistories_thenReturn() {
        String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
        Pageable pageable = PageRequest.of(0, 10);
        StakeWithdrawalProjection swp = Mockito.mock(StakeWithdrawalProjection.class);
        when(swp.getAmount()).thenReturn(BigInteger.ONE);
        when(swp.getEpochNo()).thenReturn(400);

        when(withdrawalRepository.getWithdrawalByAddress(stakeKey, pageable)).thenReturn(new PageImpl<>(List.of(swp)));

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
        StakeInstantaneousRewardsProjection sirp1 = Mockito.mock(StakeInstantaneousRewardsProjection.class);
        when(sirp1.getBlockNo()).thenReturn(1L);
        when(sirp1.getBlockIndex()).thenReturn(1);
        StakeInstantaneousRewardsProjection sirp2 = Mockito.mock(StakeInstantaneousRewardsProjection.class);
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
    void testGetTopDelegators_thenReturn() {
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

        when(stakeAddressRepository.findStakeAddressOrderByBalance(pageable)).thenReturn(List.of(sap));
        when(fetchRewardDataService.checkRewardAvailable(List.of("address"))).thenReturn(true);
        when(withdrawalRepository.getRewardWithdrawnByAddrIn(any())).thenReturn(List.of(swp));
        when(rewardRepository.getTotalRewardByStakeAddressIn(any())).thenReturn(List.of(srp));
        when(delegationRepository.findPoolDataByAddressIn(Set.of("address"))).thenReturn(List.of(sdp));
        when(stakeAddressMapper.fromStakeAddressAndDelegationProjection(any(), any())).thenReturn(StakeFilterResponse.builder().build());

        var response = stakeKeyService.getTopDelegators(pageable);
        assertEquals(response.getTotalItems(), 10);
        assertEquals(response.getTotalPages(), 1);
        assertEquals(response.getCurrentPage(), 0);
        assertEquals(response.getData().get(0).getBalance(), BigInteger.ONE);
    }

    @Test
    void testGetTopDelegators_thenReturnKoiOs() {
        Pageable pageable = PageRequest.of(0, 10);
        StakeAddressProjection sap = Mockito.mock(StakeAddressProjection.class);
        when(sap.getStakeAddress()).thenReturn("address");

        when(stakeAddressRepository.findStakeAddressOrderByBalance(pageable)).thenReturn(List.of(sap));
        when(fetchRewardDataService.checkRewardAvailable(List.of("address"))).thenReturn(false);
        when(fetchRewardDataService.fetchReward(List.of("address"))).thenReturn(false);

        assertThrows(FetchRewardException.class, () -> stakeKeyService.getTopDelegators(pageable));
    }

    @Test
    void testGetAddresses_thenReturn() {
        String stakeKey = "stake1ux7n7hxpt43f4au4w3dmwudk9u25yp7xsrvdj0426fs297sys3lyx";
        Pageable pageable = PageRequest.of(0, 10);
        Address address = Address.builder().address("address").balance(BigInteger.ONE).build();


        when(addressRepository.findByStakeAddress(stakeKey, pageable)).thenReturn(new PageImpl<>(List.of(address)));
        when(addressMapper.fromAddressToFilterResponse(address)).thenReturn(AddressFilterResponse.builder().address("address").balance(BigInteger.ONE).build());

        var response = stakeKeyService.getAddresses(stakeKey, pageable);
        assertEquals(response.getTotalItems(), 1);
        assertEquals(response.getTotalPages(), 1);
        assertEquals(response.getCurrentPage(), 0);
        assertEquals(response.getData().get(0).getAddress(), "address");
        assertEquals(response.getData().get(0).getBalance(), BigInteger.ONE);
    }

    @Test
    void testGetStakeAnalytics_thenReturn() {
        ReflectionTestUtils.setField(stakeKeyService, "network", "mainnet");
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));
        when(fetchRewardDataService.isKoiOs()).thenReturn(false);
        when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        when(valueOperations.get("TOTAL_ACTIVATE_STAKE_mainnet_400")).thenReturn(BigInteger.ONE);
        when(valueOperations.get("TOTAL_LIVE_STAKE_mainnet")).thenReturn(BigInteger.TWO);

        var response = stakeKeyService.getStakeAnalytics();
        assertEquals(response.getActiveStake(), BigInteger.ONE);
        assertEquals(response.getLiveStake(), BigInteger.TWO);
    }

    @Test
    void testGetStakeAnalytics_thenReturnKoiOs() {
        ReflectionTestUtils.setField(stakeKeyService, "network", "mainnet");
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));
        when(fetchRewardDataService.isKoiOs()).thenReturn(true);
        when(poolInfoRepository.getTotalActiveStake(400)).thenReturn(BigInteger.ONE);
        when(poolInfoRepository.getTotalLiveStake(400)).thenReturn(BigInteger.TWO);

        var response = stakeKeyService.getStakeAnalytics();
        assertEquals(response.getActiveStake(), BigInteger.ONE);
        assertEquals(response.getLiveStake(), BigInteger.TWO);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnV1() throws ExecutionException, InterruptedException {
        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_DAY;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.of(localDate));
        when(aggregateAddressTxBalanceRepository.sumBalanceByStakeAddressId(any(), any())).thenReturn(Optional.of(BigInteger.ONE));
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any(), any())).thenReturn(Optional.of(BigInteger.ONE));


        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnV2() throws ExecutionException, InterruptedException {

        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_WEEK;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.of(localDate));
        when(aggregateAddressTxBalanceRepository.sumBalanceByStakeAddressId(any(), any())).thenReturn(Optional.of(BigInteger.ONE));
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any(), any())).thenReturn(Optional.of(BigInteger.ONE));

        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnV3() throws ExecutionException, InterruptedException {
        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_DAY;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any())).thenReturn(Optional.of(BigInteger.ONE));

        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnV4() throws ExecutionException, InterruptedException {
        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_WEEK;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any())).thenReturn(Optional.of(BigInteger.ONE));

        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnBalanceZero() throws ExecutionException, InterruptedException {
        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_WEEK;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any())).thenReturn(Optional.of(BigInteger.ZERO));

        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeBalanceAnalytics_thenReturnBalanceZeroV2() throws ExecutionException, InterruptedException {
        String stakeKey = "stake_key";
        AnalyticType type = AnalyticType.ONE_DAY;
        StakeAddress stakeAddress = StakeAddress.builder().id(1L).build();
        LocalDate localDate = LocalDate.now().minusDays(5);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(stakeAddress));
        when(aggregateAddressTxBalanceRepository.getMaxDay()).thenReturn(Optional.empty());
        when(addressTxBalanceRepository.getBalanceByStakeAddressAndTime(any(), any())).thenReturn(Optional.of(BigInteger.ZERO));


        var response = stakeKeyService.getStakeBalanceAnalytics(stakeKey, type);
        assertNotNull(response);
    }

    @Test
    void testGetStakeRewardAnalytics_throwException() {
        String stakeKey = "stake_key";

        when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(false);
        when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(false);

        assertThrows(FetchRewardException.class, () -> stakeKeyService.getStakeRewardAnalytics(stakeKey));
    }

    @Test
    void testGetStakeRewardAnalytics_thenReturn() {
        String stakeKey = "stake_key";

        when(fetchRewardDataService.checkRewardAvailable(stakeKey)).thenReturn(true);
        when(rewardRepository.findRewardByStake(stakeKey)).thenReturn(List.of(StakeAnalyticRewardResponse.builder().epoch(1).value(BigInteger.ONE).build()));
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));

        var response = stakeKeyService.getStakeRewardAnalytics(stakeKey);
        assertNotNull(response);
    }

    @Test
    void testGetStakeMinMaxBalance_thenReturn() {
        String stakeKey = "stake_key";
        MinMaxProjection projection = Mockito.mock(MinMaxProjection.class);
        when(projection.getMinVal()).thenReturn(BigInteger.ONE);
        when(projection.getMaxVal()).thenReturn(BigInteger.TWO);

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(StakeAddress.builder().id(1L).build()));
        when(addressTxBalanceRepository.findMinMaxBalanceByStakeAddress(1L)).thenReturn(projection);

        var response = stakeKeyService.getStakeMinMaxBalance(stakeKey);
        assertEquals(response.get(0), BigInteger.ONE);
        assertEquals(response.get(1), BigInteger.TWO);
    }

    @Test
    void testGetStakeMinMaxBalance_thenReturnBalanceNull() {
        String stakeKey = "stake_key";

        when(stakeAddressRepository.findByView(stakeKey)).thenReturn(Optional.of(StakeAddress.builder().id(1L).build()));
        when(addressTxBalanceRepository.findMinMaxBalanceByStakeAddress(1L)).thenReturn(null);

        var response = stakeKeyService.getStakeMinMaxBalance(stakeKey);
        assertEquals(response.size(), 0);
    }

    @Test
    void testGetStakeByAddress_throwException() {
        String address = "wrong_address";

        assertThrows(BusinessException.class, () -> stakeKeyService.getStakeByAddress(address));
    }

    @Test
    void testRewardDistribution_thenReturnMember(){
        String stakeKey = "stake_key";

        when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(true);
        when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey)).thenReturn(Optional.of(BigInteger.valueOf(10)));
        when(rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey)).thenReturn(Set.of(RewardType.MEMBER));

        var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
        assertEquals(stakeKey,response.getStakeAddress());
        assertEquals(BigInteger.valueOf(10),response.getRewardAvailable());
        assertTrue(response.isHasMemberReward());
        assertFalse(response.isHasLeaderReward());
    }

    @Test
    void testRewardDistribution_thenReturnMemberAndLeader(){
        String stakeKey = "stake_key";

        when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(true);
        when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey)).thenReturn(Optional.of(BigInteger.valueOf(10)));
        when(rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey)).thenReturn(Set.of(RewardType.MEMBER,RewardType.LEADER));

        var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
        assertEquals(stakeKey,response.getStakeAddress());
        assertEquals(BigInteger.valueOf(10),response.getRewardAvailable());
        assertTrue(response.isHasMemberReward());
        assertTrue(response.isHasLeaderReward());
    }

    @Test
    void testRewardDistribution_thenReturnNoType(){
        String stakeKey = "stake_key";

        when(fetchRewardDataService.fetchReward(stakeKey)).thenReturn(true);
        when(rewardRepository.getAvailableRewardByStakeAddress(stakeKey)).thenReturn(Optional.of(BigInteger.valueOf(10)));
        when(rewardRepository.getAllRewardTypeOfAStakeKey(stakeKey)).thenReturn(new HashSet<>());

        var response = stakeKeyService.getStakeAddressRewardDistributionInfo(stakeKey);
        assertEquals(stakeKey,response.getStakeAddress());
        assertEquals(BigInteger.valueOf(10),response.getRewardAvailable());
        assertFalse(response.isHasMemberReward());
        assertFalse(response.isHasLeaderReward());
    }
}