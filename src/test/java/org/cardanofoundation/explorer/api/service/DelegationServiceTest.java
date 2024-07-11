package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import org.mockito.Answers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.common.enumeration.PoolStatus;
import org.cardanofoundation.explorer.api.model.request.pool.PoolListFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.chart.PoolDetailAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiosProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregatePoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.impl.DelegationServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.AggregatePoolInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.Delegation_;
import org.cardanofoundation.explorer.common.entity.ledgersync.Epoch;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolHash;

@ExtendWith(MockitoExtension.class)
class DelegationServiceTest {

  @Mock private DelegationRepository delegationRepository;

  @Mock private BlockRepository blockRepository;

  @Mock private EpochRepository epochRepository;

  @Mock private EpochStakeRepository epochStakeRepository;

  @Mock private PoolHashRepository poolHashRepository;

  @Mock private StakeAddressBalanceRepository stakeAddressBalanceRepository;

  @Mock private PoolUpdateRepository poolUpdateRepository;

  @Mock private RewardRepository rewardRepository;

  @Mock private PoolInfoRepository poolInfoRepository;

  @Mock private PoolHistoryRepository poolHistoryRepository;

  @Mock(answer = Answers.RETURNS_DEEP_STUBS)
  private RedisTemplate<String, Object> redisTemplate;

  @Mock private FetchRewardDataService fetchRewardDataService;

  @Mock private StakeAddressRepository stakeAddressRepository;

  @Mock private WithdrawalRepository withdrawalRepository;

  @Mock private TxRepository txRepository;

  @Mock private EpochService epochService;

  @Mock private AggregatePoolInfoRepository aggregatePoolInfoRepository;

  @Mock private PoolCertificateService poolCertificateService;

  @InjectMocks private DelegationServiceImpl delegationService;

  @BeforeEach
  void preSetup() {
    ReflectionTestUtils.setField(delegationService, "defaultSize", 20);
  }

  @Test
  void getDelegations_ShouldReturnDelegationResponse() {
    // Arrange
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.DESC, "id");

    List<Long> txIds = List.of(1L, 2L, 3L);

    TxIOProjection tx1 = Mockito.mock(TxIOProjection.class);
    when(tx1.getId()).thenReturn(1L);
    when(tx1.getHash()).thenReturn("hash1");
    when(tx1.getEpochNo()).thenReturn(1);
    when(tx1.getEpochSlotNo()).thenReturn(1);
    when(tx1.getBlockNo()).thenReturn(1L);
    TxIOProjection tx2 = Mockito.mock(TxIOProjection.class);
    when(tx2.getId()).thenReturn(2L);
    when(tx2.getHash()).thenReturn("hash2");
    when(tx2.getEpochNo()).thenReturn(2);
    when(tx2.getEpochSlotNo()).thenReturn(2);
    when(tx2.getBlockNo()).thenReturn(2L);
    TxIOProjection tx3 = Mockito.mock(TxIOProjection.class);
    when(tx3.getId()).thenReturn(3L);
    when(tx3.getHash()).thenReturn("hash3");
    when(tx3.getEpochNo()).thenReturn(3);
    when(tx3.getEpochSlotNo()).thenReturn(3);
    when(tx3.getBlockNo()).thenReturn(3L);
    List<TxIOProjection> txs = List.of(tx1, tx2, tx3);
    DelegationProjection d1 = Mockito.mock(DelegationProjection.class);
    when(d1.getTxId()).thenReturn(1L);
    when(d1.getPoolView()).thenReturn("view1");
    when(d1.getPoolName()).thenReturn("name1");
    when(d1.getStakeAddress()).thenReturn("stakeAddress1");
    when(d1.getTickerName()).thenReturn("ticket1");
    DelegationProjection d2 = Mockito.mock(DelegationProjection.class);
    when(d2.getTxId()).thenReturn(2L);
    when(d2.getPoolView()).thenReturn("view2");
    when(d2.getPoolName()).thenReturn("name2");
    when(d2.getStakeAddress()).thenReturn("stakeAddress2");
    when(d2.getTickerName()).thenReturn("ticket2");
    DelegationProjection d3 = Mockito.mock(DelegationProjection.class);
    when(d3.getTxId()).thenReturn(3L);
    when(d3.getPoolView()).thenReturn("view3");
    when(d3.getPoolName()).thenReturn("name3");
    when(d3.getStakeAddress()).thenReturn("stakeAddress3");
    when(d3.getTickerName()).thenReturn("ticket3");
    List<DelegationProjection> delegations = List.of(d1, d2, d3);

    when(delegationRepository.findAllDelegations(pageable)).thenReturn(new PageImpl<>(txIds));
    when(txRepository.findTxIn(txIds)).thenReturn(txs);
    when(delegationRepository.findDelegationByTxIdIn(txIds)).thenReturn(delegations);

    // Act
    BaseFilterResponse<DelegationResponse> response = delegationService.getDelegations(pageable);

    // Assert
    verify(delegationRepository).findAllDelegations(pageable);
    verify(txRepository).findTxIn(txIds);
    verify(delegationRepository).findDelegationByTxIdIn(txIds);

    assertEquals(txIds.size(), response.getData().size());
    // Add more assertions based on the expected behavior of the method
  }

  @Test
  void testGetDataForDelegationHeader_shouldReturn() {
    // Mock dependencies
    EpochSummary epochSummary =
        EpochSummary.builder()
            .no(1)
            .slot(0)
            .startTime(LocalDateTime.now())
            .endTime(LocalDateTime.now().plusDays(5))
            .totalSlot(432000)
            .account(1)
            .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);
    when(fetchRewardDataService.checkAdaPots(anyInt())).thenReturn(false);
    when(fetchRewardDataService.fetchAdaPots(any())).thenReturn(true);
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(poolInfoRepository.getTotalLiveStake(anyInt())).thenReturn(BigInteger.TEN);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo()); // Update this based on the actual calculation
    assertEquals(BigInteger.TEN, response.getLiveStake());
    assertEquals(0, response.getDelegators());

    // Verify interactions with dependencies
    verify(fetchRewardDataService).checkAdaPots(1);
    verify(fetchRewardDataService).useKoios();
    verify(poolInfoRepository).getTotalLiveStake(1);
  }

  @Test
  void testGetDataForDelegationHeader_shouldReturnV2() {
    // Mock dependencies
    EpochSummary epochSummary =
        EpochSummary.builder()
            .no(1)
            .slot(0)
            .startTime(LocalDateTime.now())
            .endTime(LocalDateTime.now().plusDays(5))
            .totalSlot(432000)
            .account(1)
            .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(redisTemplate.opsForValue().get(any())).thenReturn(10);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo()); // Update this based on the actual calculation
    assertEquals(null, response.getLiveStake());
    assertEquals(10, response.getDelegators());
  }

  @Test
  void testGetDataForDelegationHeader_shouldReturnRedisIsNull() {
    // Mock dependencies
    EpochSummary epochSummary =
        EpochSummary.builder()
            .no(1)
            .slot(0)
            .startTime(LocalDateTime.now())
            .endTime(LocalDateTime.now().plusDays(5))
            .totalSlot(432000)
            .account(1)
            .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);
    when(fetchRewardDataService.useKoios()).thenReturn(false);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo()); // Update this based on the actual calculation
    assertEquals(null, response.getLiveStake());
    assertEquals(0, response.getDelegators());

    // Verify interactions with dependencies
    verify(fetchRewardDataService).useKoios();
  }

  @Test
  void testGetDataForPoolTable_withSearch() {
    String search = "example";
    PoolListFilter poolListFilter =
        PoolListFilter.builder().query(search).isShowRetired(true).build();
    // Mocked input data
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.ASC, "id");

    List<Object> poolRetiredIds = new ArrayList<>();
    poolRetiredIds.add(-1L);
    // Mocked repository responses
    List<PoolListProjection> poolIdPageContent = new ArrayList<>();
    PoolListProjection projection = Mockito.mock(PoolListProjection.class);
    when(projection.getPoolView()).thenReturn("view");
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getPledge()).thenReturn(BigInteger.ONE);
    poolIdPageContent.add(projection);
    // Add mock data to poolIdPageContent

    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(redisTemplate.opsForHash().values(any())).thenReturn(poolRetiredIds);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(100));
    when(poolHashRepository.findAllWithUsingKoiOs(
            any(),
            anyCollection(),
            anyInt(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any()))
        .thenReturn(new PageImpl<>(poolIdPageContent));
    // Call the method
    BaseFilterResponse<PoolResponse> response =
        delegationService.getDataForPoolTable(pageable, poolListFilter);

    // Perform assertions
    assertNotNull(response);
    assertEquals(poolIdPageContent.size(), response.getData().size());
    Assertions.assertEquals(1, response.getData().get(0).getId());
    // Add more assertions as needed
  }

  @Test
  void testGetDataForPoolTable_withoutSearch() {
    PoolListFilter poolListFilter =
        PoolListFilter.builder().query(null).isShowRetired(true).build();
    // Mocked input data
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.ASC, "id");
    String search = "";
    // Mocked repository responses
    List<PoolListProjection> poolIdPageContent = new ArrayList<>();
    // Add mock data to poolIdPageContent
    Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(poolHashRepository.findAllWithUsingKoiOs(
            any(),
            anyCollection(),
            anyInt(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any()))
        .thenReturn(new PageImpl<>(poolIdPageContent));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(400));
    // Call the method
    BaseFilterResponse<PoolResponse> response =
        delegationService.getDataForPoolTable(pageable, poolListFilter);

    // Perform assertions
    assertNotNull(response);
    assertEquals(poolIdPageContent.size(), response.getData().size());
    // Add more assertions as needed
  }

  @Test
  void testGetDataForPoolDetail_notUseKoios_thenReturn() {
    // Mocking dependencies
    Timestamp timestamp = Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS));
    Integer currentEpochNo = 1;
    Epoch epoch = new Epoch();
    epoch.setNo(currentEpochNo);
    epoch.setStartTime(Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS)));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpochNo));

    String poolView = "poolView";
    Long poolId = 1L;
    PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
    when(projection.getPoolName()).thenReturn(poolView);
    when(projection.getPoolId()).thenReturn(poolId);
    when(projection.getPoolView()).thenReturn(poolView);

    AggregatePoolInfo aggregatePoolInfo =
        AggregatePoolInfo.builder()
            .poolId(poolId)
            .delegatorCount(10)
            .blockInEpoch(epoch.getNo())
            .blockLifeTime(20)
            .build();
    // Mocking fetchRewardDataService
    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(poolHashRepository.getDataForPoolDetailNoReward(poolView, currentEpochNo))
        .thenReturn(projection);
    when(poolUpdateRepository.getCreatedTimeOfPool(poolId)).thenReturn(timestamp);
    when(poolUpdateRepository.findOwnerAccountByPool(poolId)).thenReturn(ownerAddress);
    when(aggregatePoolInfoRepository.findByPoolId(1L)).thenReturn(aggregatePoolInfo);
    when(poolCertificateService.getCurrentPoolStatus(anyString())).thenReturn(PoolStatus.ACTIVE);
    // Execute the function
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

    // Verify the interactions
    verify(epochRepository).findCurrentEpochNo();
    verify(poolHashRepository).getDataForPoolDetailNoReward(poolView, currentEpochNo);
    verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
    verify(poolUpdateRepository).findOwnerAccountByPool(poolId);

    assertEquals(poolView, result.getPoolName());
    Assertions.assertEquals(10, result.getDelegators());
    Assertions.assertTrue(result.getOwnerAccounts().contains("address"));
  }

  @Test
  void testGetDataForPoolDetail_useKoios_thenReturn() {
    // Mocking dependencies
    Timestamp timestamp = Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS));
    Integer currentEpochNo = 2;
    Epoch epoch = new Epoch();
    epoch.setNo(currentEpochNo);
    epoch.setStartTime(Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS)));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpochNo));

    String poolView = "poolView";
    Long poolId = 1L;
    BigInteger reserves = BigInteger.valueOf(1000);
    Integer paramK = 2;
    PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
    when(projection.getPoolName()).thenReturn(poolView);
    when(projection.getPoolId()).thenReturn(poolId);
    when(projection.getReserves()).thenReturn(reserves);
    when(projection.getParamK()).thenReturn(paramK);
    when(projection.getPoolView()).thenReturn(poolView);
    AggregatePoolInfo aggregatePoolInfo =
        AggregatePoolInfo.builder()
            .poolId(1L)
            .blockLifeTime(1)
            .delegatorCount(10)
            .blockInEpoch(1)
            .build();

    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(poolHashRepository.getDataForPoolDetail(poolView, currentEpochNo)).thenReturn(projection);
    when(fetchRewardDataService.checkAdaPots(currentEpochNo)).thenReturn(false);
    when(fetchRewardDataService.fetchAdaPots(List.of(currentEpochNo))).thenReturn(true);
    when(aggregatePoolInfoRepository.findByPoolId(poolId)).thenReturn(aggregatePoolInfo);
    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(poolUpdateRepository.getCreatedTimeOfPool(poolId)).thenReturn(timestamp);
    when(poolUpdateRepository.findOwnerAccountByPool(poolId)).thenReturn(ownerAddress);
    when(stakeAddressBalanceRepository.getBalanceByView(ownerAddress)).thenReturn(BigInteger.TEN);
    when(rewardRepository.getAvailableRewardByAddressList(ownerAddress)).thenReturn(BigInteger.TEN);
    when(withdrawalRepository.getRewardWithdrawnByAddressList(ownerAddress))
        .thenReturn(BigInteger.TEN);

    when(poolCertificateService.getCurrentPoolStatus(anyString())).thenReturn(PoolStatus.ACTIVE);
    // Execute the function
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

    // Verify the interactions
    verify(epochRepository).findCurrentEpochNo();
    verify(poolHashRepository).getDataForPoolDetail(poolView, currentEpochNo);
    verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
    verify(poolUpdateRepository).findOwnerAccountByPool(poolId);

    assertEquals(poolView, result.getPoolName());
    Assertions.assertTrue(result.getOwnerAccounts().contains("address"));
    Assertions.assertTrue(PoolStatus.ACTIVE.equals(result.getPoolStatus()));
  }

  @Test
  void testGetEpochListForPoolDetail_shouldReturnKoios() {
    // Mocking the necessary data
    PageRequest pageable = PageRequest.of(0, 10);
    String poolView = "pool123";
    Set<String> poolViews = Collections.singleton(poolView);
    PoolHistoryKoiosProjection phkp = Mockito.mock(PoolHistoryKoiosProjection.class);
    when(phkp.getDelegateReward()).thenReturn(BigInteger.ONE);
    when(phkp.getRos()).thenReturn(1D);
    when(phkp.getEpochNo()).thenReturn(400);
    Page<PoolHistoryKoiosProjection> poolHistoryKoiosProjections = new PageImpl<>(List.of(phkp));
    PoolDetailEpochProjection pdep = Mockito.mock(PoolDetailEpochProjection.class);
    when(pdep.getEpochNo()).thenReturn(400);
    when(pdep.getCountBlock()).thenReturn(1L);
    List<PoolDetailEpochProjection> poolDetailEpochProjections = List.of(pdep);

    when(poolHashRepository.findByViewOrHashRaw(poolView))
        .thenReturn(Optional.of(PoolHash.builder().id(1L).view(poolView).build()));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.checkPoolHistoryForPool(poolViews)).thenReturn(false);
    when(fetchRewardDataService.fetchPoolHistoryForPool(poolViews)).thenReturn(true);
    when(poolHistoryRepository.getPoolHistoryKoios(poolView, pageable))
        .thenReturn(poolHistoryKoiosProjections);
    when(poolHashRepository.findEpochByPool(1L, Set.of(400)))
        .thenReturn(poolDetailEpochProjections);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(440));

    var response = delegationService.getEpochListForPoolDetail(pageable, poolView);
    var expect =
        new BaseFilterResponse<>(
            List.of(
                PoolDetailEpochResponse.builder()
                    .epoch(400)
                    .block(1L)
                    .delegators(BigInteger.ONE)
                    .ros(1D)
                    .build()),
            1L);

    assert response.getTotalPages() == 1;
    assert response.getTotalItems() == 1L;
    assert response.getCurrentPage() == 0;
    assert response.getData().size() == 1;
    assert Objects.equals(response.getData().get(0).getEpoch(), expect.getData().get(0).getEpoch());
    assert Objects.equals(response.getData().get(0).getBlock(), expect.getData().get(0).getBlock());
    assert Objects.equals(
        response.getData().get(0).getDelegators(), expect.getData().get(0).getDelegators());
    assert Objects.equals(response.getData().get(0).getRos(), expect.getData().get(0).getRos());
  }

  @Test
  void testGetEpochListForPoolDetail_shouldReturnKoiosV2() {
    // Mocking the necessary data
    PageRequest pageable = PageRequest.of(0, 10);
    String poolView = "pool123";
    Set<String> poolViews = Collections.singleton(poolView);
    PoolHistoryKoiosProjection phkp = Mockito.mock(PoolHistoryKoiosProjection.class);
    when(phkp.getDelegateReward()).thenReturn(BigInteger.ONE);
    when(phkp.getRos()).thenReturn(1D);
    when(phkp.getEpochNo()).thenReturn(400);
    Page<PoolHistoryKoiosProjection> poolHistoryKoiosProjections = new PageImpl<>(List.of(phkp));
    PoolDetailEpochProjection pdep = Mockito.mock(PoolDetailEpochProjection.class);
    when(pdep.getEpochNo()).thenReturn(400);
    when(pdep.getCountBlock()).thenReturn(1L);
    List<PoolDetailEpochProjection> poolDetailEpochProjections = List.of(pdep);

    when(poolHashRepository.findByViewOrHashRaw(poolView))
        .thenReturn(Optional.of(PoolHash.builder().id(1L).view(poolView).build()));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.checkPoolHistoryForPool(poolViews)).thenReturn(true);
    when(poolHistoryRepository.getPoolHistoryKoios(poolView, pageable))
        .thenReturn(poolHistoryKoiosProjections);
    when(poolHashRepository.findEpochByPool(1L, Set.of(400)))
        .thenReturn(poolDetailEpochProjections);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(440));

    var response = delegationService.getEpochListForPoolDetail(pageable, poolView);
    var expect =
        new BaseFilterResponse<>(
            List.of(
                PoolDetailEpochResponse.builder()
                    .epoch(400)
                    .block(1L)
                    .delegators(BigInteger.ONE)
                    .ros(1D)
                    .build()),
            1L);

    assert response.getTotalPages() == 1;
    assert response.getTotalItems() == 1L;
    assert response.getCurrentPage() == 0;
    assert response.getData().size() == 1;
    assert Objects.equals(response.getData().get(0).getEpoch(), expect.getData().get(0).getEpoch());
    assert Objects.equals(response.getData().get(0).getBlock(), expect.getData().get(0).getBlock());
    assert Objects.equals(
        response.getData().get(0).getDelegators(), expect.getData().get(0).getDelegators());
    assert Objects.equals(response.getData().get(0).getRos(), expect.getData().get(0).getRos());
  }

  @Test
  void testGetAnalyticsForPoolDetail_notUseKoios() {
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    PoolDetailAnalyticsResponse response = delegationService.getAnalyticsForPoolDetail("poolView");

    Assertions.assertEquals(null, response.getDelegatorChart());
    Assertions.assertEquals(null, response.getEpochChart());
  }

  @Test
  void testGetDelegatorsForPoolDetail() {
    // Mock data
    Pageable pageable = Pageable.unpaged();
    Long txId1 = 1L;
    Long txId2 = 2L;
    TxIOProjection tx1 = Mockito.mock(TxIOProjection.class);
    when(tx1.getId()).thenReturn(txId1);
    when(tx1.getHash()).thenReturn("hash1");
    when(tx1.getEpochNo()).thenReturn(1);
    when(tx1.getEpochSlotNo()).thenReturn(1);
    when(tx1.getBlockNo()).thenReturn(1L);
    TxIOProjection tx2 = Mockito.mock(TxIOProjection.class);
    when(tx2.getId()).thenReturn(txId2);
    when(tx2.getHash()).thenReturn("hash2");
    when(tx2.getEpochNo()).thenReturn(2);
    when(tx2.getEpochSlotNo()).thenReturn(2);
    when(tx2.getBlockNo()).thenReturn(2L);
    List<TxIOProjection> txs = Arrays.asList(tx1, tx2);
    DelegationProjection d1 = Mockito.mock(DelegationProjection.class);
    when(d1.getTxId()).thenReturn(txId1);
    when(d1.getStakeAddress()).thenReturn("stakeAddress1");
    when(d1.getPoolName()).thenReturn("poolName1");
    when(d1.getTickerName()).thenReturn("tickerName1");
    when(d1.getPoolView()).thenReturn("poolView1");
    DelegationProjection d2 = Mockito.mock(DelegationProjection.class);
    when(d2.getTxId()).thenReturn(txId2);
    when(d2.getStakeAddress()).thenReturn("stakeAddress2");
    when(d2.getPoolName()).thenReturn("poolName2");
    when(d2.getTickerName()).thenReturn("tickerName2");
    when(d2.getPoolView()).thenReturn("poolView2");
    List<DelegationProjection> delegations = Arrays.asList(d1, d2);

    // Mock repository methods
    Page<Long> txIdPage = new PageImpl<>(List.of(txId1, txId2));
    when(delegationRepository.findAllDelegations(pageable)).thenReturn(txIdPage);
    when(txRepository.findTxIn(txIdPage.getContent())).thenReturn(txs);
    when(delegationRepository.findDelegationByTxIdIn(txIdPage.getContent()))
        .thenReturn(delegations);

    // Call the service method
    BaseFilterResponse<DelegationResponse> response = delegationService.getDelegations(pageable);

    // Assertions
    assertEquals(2, response.getData().size());
    assertEquals(tx1.getHash(), response.getData().get(0).getTxHash());
    assertEquals(tx2.getHash(), response.getData().get(1).getTxHash());
    // Add more assertions as needed
  }

  @Test
  void testFindTopDelegationPool_shouldReturn() {
    PageRequest pageable = PageRequest.of(0, 20);
    // Mock data
    PoolDelegationSummaryProjection pds1 = Mockito.mock(PoolDelegationSummaryProjection.class);
    when(pds1.getPoolName()).thenReturn("Pool 1");
    when(pds1.getPoolView()).thenReturn("pool1");
    when(pds1.getPledge()).thenReturn(BigInteger.valueOf(100));
    when(pds1.getPoolId()).thenReturn(1L);
    PoolDelegationSummaryProjection pds2 = Mockito.mock(PoolDelegationSummaryProjection.class);
    when(pds2.getPoolName()).thenReturn("Pool 2");
    when(pds2.getPoolView()).thenReturn("pool2");
    when(pds2.getPledge()).thenReturn(BigInteger.valueOf(200));
    when(pds2.getPoolId()).thenReturn(2L);
    List<PoolDelegationSummaryProjection> pools = List.of(pds1, pds2);

    PoolCountProjection pcp1 = Mockito.mock(PoolCountProjection.class);
    when(pcp1.getPoolId()).thenReturn(1L);
    when(pcp1.getPoolView()).thenReturn("pool1");
    when(pcp1.getCountValue()).thenReturn(1);
    PoolCountProjection pcp2 = Mockito.mock(PoolCountProjection.class);
    when(pcp2.getPoolId()).thenReturn(2L);
    when(pcp2.getPoolView()).thenReturn("pool2");
    when(pcp2.getCountValue()).thenReturn(2);
    List<PoolCountProjection> poolCountProjections = List.of(pcp1, pcp2);

    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));
    when(delegationRepository.findDelegationPoolsSummary(Mockito.anySet())).thenReturn(pools);
    when(redisTemplate.opsForHash().multiGet(any(), any()))
        .thenReturn(List.of(BigInteger.valueOf(1000), BigInteger.valueOf(2000)));
    when(blockRepository.findTopDelegationByEpochBlock(1, pageable))
        .thenReturn(poolCountProjections);

    // Call the method
    List<PoolResponse> result = delegationService.findTopDelegationPool(PageRequest.of(0, 21));

    // Verify the result
    assert result.size() == 2;
    assert result.get(0).getPoolId().equals("pool2");
    assert result.get(0).getPoolName().equals("Pool 2");
    assert result.get(0).getPledge().equals(BigInteger.valueOf(200));
    assert result.get(1).getPoolId().equals("pool1");
    assert result.get(1).getPoolName().equals("Pool 1");
    assert result.get(1).getPledge().equals(BigInteger.valueOf(100));
  }

  @Test
  void testFindTopDelegationPool_shouldReturnKoios() {
    PageRequest pageable = PageRequest.of(0, 20);
    // Mock data
    int currentEpoch = 2;
    PoolDelegationSummaryProjection pds = Mockito.mock(PoolDelegationSummaryProjection.class);
    when(pds.getPoolName()).thenReturn("Pool 1");
    when(pds.getPoolView()).thenReturn("pool1");
    when(pds.getPledge()).thenReturn(BigInteger.valueOf(100));
    when(pds.getPoolId()).thenReturn(1L);
    List<PoolDelegationSummaryProjection> pools = List.of(pds);

    PoolCountProjection pcp = Mockito.mock(PoolCountProjection.class);
    when(pcp.getPoolId()).thenReturn(1L);
    when(pcp.getPoolView()).thenReturn("pool1");
    when(pcp.getCountValue()).thenReturn(1);
    List<PoolCountProjection> poolCountProjections = List.of(pcp);

    Set<String> poolIds = Set.of("pool1");

    when(blockRepository.findTopDelegationByEpochBlock(currentEpoch, pageable))
        .thenReturn(poolCountProjections);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(poolHashRepository.getListPoolIdIn(poolIds)).thenReturn(Set.of(1L));
    when(delegationRepository.findDelegationPoolsSummary(Set.of(1L))).thenReturn(pools);
    when(blockRepository.getCountBlockByPools(Set.of(1L))).thenReturn(poolCountProjections);
    // Call the method
    List<PoolResponse> result = delegationService.findTopDelegationPool(PageRequest.of(0, 21));

    // Verify the result
    assert result.size() == 1;
    assert result.get(0).getPoolId().equals("pool1");
    assert result.get(0).getPoolName().equals("Pool 1");
    assert result.get(0).getPledge().equals(BigInteger.valueOf(100));
  }

  @Test
  void testGetAnalyticsForPoolDetail_notUseKoios_shouldReturn() {
    String poolView = "poolView";
    when(fetchRewardDataService.useKoios()).thenReturn(false);

    var response = delegationService.getAnalyticsForPoolDetail(poolView);

    Assertions.assertNull(response.getEpochChart());
    Assertions.assertNull(response.getDelegatorChart());
  }

  @Test
  void testGetAnalyticsForPoolDetail_shouldReturnKoios() {
    String poolView = "poolView";
    Long poolId = 1L;
    Set<String> poolViews = Collections.singleton(poolView);
    EpochChartProjection ecp = Mockito.mock(EpochChartProjection.class);
    when(ecp.getChartValue()).thenReturn(BigInteger.ONE);
    DelegatorChartProjection dcp = Mockito.mock(DelegatorChartProjection.class);
    when(dcp.getChartValue()).thenReturn(1L);

    when(poolHashRepository.findByViewOrHashRaw(poolView))
        .thenReturn(Optional.of(PoolHash.builder().id(poolId).view(poolView).build()));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.checkPoolHistoryForPool(poolViews)).thenReturn(false);
    when(fetchRewardDataService.fetchPoolHistoryForPool(poolViews)).thenReturn(true);
    when(poolHistoryRepository.getPoolHistoryKoiosForEpochChart(poolView)).thenReturn(List.of(ecp));
    when(poolHistoryRepository.getDataForDelegatorChart(any())).thenReturn(List.of(dcp));

    var response = delegationService.getAnalyticsForPoolDetail(poolView);

    assert Objects.equals(response.getDelegatorChart().getHighest(), 1L);
    assert Objects.equals(response.getDelegatorChart().getLowest(), 1L);
    assert Objects.equals(response.getDelegatorChart().getDataByDays().size(), 1);
    assert Objects.equals(response.getEpochChart().getHighest(), BigInteger.ONE);
    assert Objects.equals(response.getEpochChart().getLowest(), BigInteger.ONE);
    assert Objects.equals(response.getEpochChart().getDataByDays().size(), 1);
  }

  @Test
  void testGetAnalyticsForPoolDetail_shouldReturnKoiosV2() {
    String poolView = "poolView";
    Long poolId = 1L;
    Set<String> poolViews = Collections.singleton(poolView);
    EpochChartProjection ecp = Mockito.mock(EpochChartProjection.class);
    when(ecp.getChartValue()).thenReturn(BigInteger.ONE);
    DelegatorChartProjection dcp = Mockito.mock(DelegatorChartProjection.class);
    when(dcp.getChartValue()).thenReturn(1L);

    when(poolHashRepository.findByViewOrHashRaw(poolView))
        .thenReturn(Optional.of(PoolHash.builder().id(poolId).view(poolView).build()));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(fetchRewardDataService.checkPoolHistoryForPool(poolViews)).thenReturn(true);
    when(poolHistoryRepository.getPoolHistoryKoiosForEpochChart(poolView)).thenReturn(List.of(ecp));
    when(poolHistoryRepository.getDataForDelegatorChart(any())).thenReturn(List.of(dcp));

    var response = delegationService.getAnalyticsForPoolDetail(poolView);

    assert Objects.equals(response.getDelegatorChart().getHighest(), 1L);
    assert Objects.equals(response.getDelegatorChart().getLowest(), 1L);
    assert Objects.equals(response.getDelegatorChart().getDataByDays().size(), 1);
    assert Objects.equals(response.getEpochChart().getHighest(), BigInteger.ONE);
    assert Objects.equals(response.getEpochChart().getLowest(), BigInteger.ONE);
    assert Objects.equals(response.getEpochChart().getDataByDays().size(), 1);
  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturn() {
    Pageable pageable =
        PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";
    Timestamp timestamp = new Timestamp(Instant.now().toEpochMilli());
    PoolDetailDelegatorProjection pddp = Mockito.mock(PoolDetailDelegatorProjection.class);
    when(pddp.getStakeAddressId()).thenReturn(1L);
    when(pddp.getTime()).thenReturn(timestamp);
    when(pddp.getFee()).thenReturn(BigInteger.ONE);
    when(pddp.getView()).thenReturn("view");
    Set<Long> addressIds = Set.of(1L);
    List<String> addressViews = List.of("view");

    when(delegationRepository.liveDelegatorsList(poolView, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(1L)));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));
    when(delegationRepository.getDelegatorsByAddress(addressIds)).thenReturn(List.of(pddp));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(stakeAddressRepository.getViewByAddressId(addressIds)).thenReturn(addressViews);
    when(fetchRewardDataService.checkEpochStakeForPool(addressViews)).thenReturn(false);
    when(fetchRewardDataService.fetchEpochStakeForPool(addressViews)).thenReturn(null);

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);

    assert response.getTotalPages() == 1;
    assert response.getTotalItems() == 1L;
    assert response.getCurrentPage() == 0;
    assert response.getData().get(0).getStakeAddressId() == 1L;
    assert response.getData().get(0).getTime() == timestamp;
    assert Objects.equals(response.getData().get(0).getFee(), BigInteger.ONE);
    assert response.getData().get(0).getView().equals("view");
  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturnV2() {
    Pageable pageable =
        PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";
    Timestamp timestamp = new Timestamp(Instant.now().toEpochMilli());
    int currentEpoch = 1;
    PoolDetailDelegatorProjection pddp = Mockito.mock(PoolDetailDelegatorProjection.class);
    when(pddp.getStakeAddressId()).thenReturn(1L);
    when(pddp.getTime()).thenReturn(timestamp);
    when(pddp.getFee()).thenReturn(BigInteger.ONE);
    when(pddp.getView()).thenReturn("view");
    Set<Long> addressIds = Set.of(1L);
    List<String> addressViews = List.of("view");
    StakeAddressProjection sap = Mockito.mock(StakeAddressProjection.class);
    when(sap.getAddress()).thenReturn(1L);
    when(sap.getTotalStake()).thenReturn(BigInteger.ONE);

    when(delegationRepository.liveDelegatorsList(poolView, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(1L)));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(delegationRepository.getDelegatorsByAddress(addressIds)).thenReturn(List.of(pddp));
    when(fetchRewardDataService.useKoios()).thenReturn(true);
    when(stakeAddressRepository.getViewByAddressId(addressIds)).thenReturn(addressViews);
    when(fetchRewardDataService.checkEpochStakeForPool(addressViews)).thenReturn(false);
    when(fetchRewardDataService.fetchEpochStakeForPool(addressViews)).thenReturn(true);
    when(epochStakeRepository.totalStakeByAddressAndPool(addressIds, currentEpoch))
        .thenReturn(List.of(sap));

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);

    assert response.getTotalPages() == 1;
    assert response.getTotalItems() == 1L;
    assert response.getCurrentPage() == 0;
    assert response.getData().get(0).getStakeAddressId() == 1L;
    assert response.getData().get(0).getTime() == timestamp;
    assert Objects.equals(response.getData().get(0).getFee(), BigInteger.ONE);
    assert response.getData().get(0).getView().equals("view");
  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturnLiveDelegatorsListIsNull() {
    Pageable pageable =
        PageRequest.of(0, 10, Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";

    when(delegationRepository.liveDelegatorsList(poolView, pageable))
        .thenReturn(new PageImpl<>(List.of()));

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);
    assert response.getTotalPages() == 0;
    assert response.getTotalItems() == 0;
    assert response.getCurrentPage() == 0;
    assert response.getData().size() == 0;
  }
}
