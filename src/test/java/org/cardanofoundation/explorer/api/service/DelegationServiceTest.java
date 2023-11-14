package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
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
import java.util.Optional;
import java.util.Set;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.DelegationResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.explorer.AggregatePoolInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochParamRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.DelegationServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation_;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.AggregatePoolInfo;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Answers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
class DelegationServiceTest {

  @Mock
  private DelegationRepository delegationRepository;

  @Mock
  private BlockRepository blockRepository;

  @Mock
  private EpochRepository epochRepository;

  @Mock
  private PoolHashRepository poolHashRepository;

  @Mock
  private PoolUpdateRepository poolUpdateRepository;

  @Mock(answer = Answers.RETURNS_DEEP_STUBS)
  private RedisTemplate<String, Object> redisTemplate;

  @Mock
  private TxRepository txRepository;

  @Mock
  private EpochService epochService;

  @Mock
  private AggregatePoolInfoRepository aggregatePoolInfoRepository;

  @InjectMocks
  private DelegationServiceImpl delegationService;

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
    EpochSummary epochSummary = EpochSummary.builder()
        .no(1)
        .slot(0)
        .startTime(LocalDateTime.now())
        .endTime(LocalDateTime.now().plusDays(5))
        .totalSlot(432000)
        .account(1)
        .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo());
    assertEquals(0, response.getDelegators());
  }

  @Test
  void testGetDataForDelegationHeader_shouldReturnV2() {
    // Mock dependencies
    EpochSummary epochSummary = EpochSummary.builder()
        .no(1)
        .slot(0)
        .startTime(LocalDateTime.now())
        .endTime(LocalDateTime.now().plusDays(5))
        .totalSlot(432000)
        .account(1)
        .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);
    when(redisTemplate.opsForValue().get(any())).thenReturn(10);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo()); // Update this based on the actual calculation
    assertNull(response.getLiveStake());
    assertEquals(10, response.getDelegators());
  }

  @Test
  void testGetDataForDelegationHeader_shouldReturnResponse() {
    // Mock dependencies
    EpochSummary epochSummary = EpochSummary.builder()
        .no(1)
        .slot(0)
        .startTime(LocalDateTime.now())
        .endTime(LocalDateTime.now().plusDays(5))
        .totalSlot(432000)
        .account(1)
        .build();
    when(epochService.getCurrentEpochSummary()).thenReturn(epochSummary);

    // Execute the method
    DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

    // Verify the results
    assertEquals(1, response.getEpochNo());
    assertEquals(0, response.getEpochSlotNo());
    assertNull(response.getLiveStake());
    assertEquals(0, response.getDelegators());
  }

  //  @Test
  void testGetDataForPoolTable_withSearch() {
    // Mocked input data
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.ASC, "id");
    String search = "example";
    List<Object> poolRetiredIds = new ArrayList<>();
    poolRetiredIds.add(-1L);
    // Mocked repository responses
    List<PoolListProjection> poolIdPageContent = new ArrayList<>();
    PoolListProjection projection = Mockito.mock(PoolListProjection.class);
    when(projection.getPoolView()).thenReturn("view");
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getPledge()).thenReturn(BigInteger.ONE);
    when(projection.getFee()).thenReturn(BigInteger.ONE);
    when(projection.getMargin()).thenReturn(1D);
    when(redisTemplate.opsForHash().values(any())).thenReturn(poolRetiredIds);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(100));
    poolIdPageContent.add(projection);
    // Add mock data to poolIdPageContent
    Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

    when(poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(any(), anyCollection(),
        any())).thenReturn(poolIdPage);
    // Call the method
    BaseFilterResponse<PoolResponse> response = delegationService.getDataForPoolTable(pageable,
        search, true);

    // Perform assertions
    assertNotNull(response);
    assertEquals(poolIdPageContent.size(), response.getData().size());
    // Add more assertions as needed
  }

  @Test
  void testGetDataForPoolTable_withoutSearch() {
    // Mocked input data
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.ASC, "id");
    String search = "";

    // Mocked repository responses
    List<PoolListProjection> poolIdPageContent = new ArrayList<>();
    // Add mock data to poolIdPageContent

    Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

    when(poolHashRepository.findAllWithoutQueryParam(any(), any())).thenReturn(poolIdPage);
    // Call the method
    BaseFilterResponse<PoolResponse> response = delegationService.getDataForPoolTable(pageable,
        search, true);

    // Perform assertions
    assertNotNull(response);
    assertEquals(poolIdPageContent.size(), response.getData().size());
  }

  @Test
  void testGetDataForPoolTable_withSearchV1() {
    // Mocked input data
    Pageable pageable = PageRequest.of(0, 10, Sort.Direction.ASC, "id");
    String search = "example";

    // Mocked repository responses
    List<PoolListProjection> poolIdPageContent = new ArrayList<>();
    PoolListProjection projection = Mockito.mock(PoolListProjection.class);
    when(projection.getPoolView()).thenReturn("view");
    when(projection.getPoolId()).thenReturn(1L);
    when(projection.getPoolName()).thenReturn("name");
    when(projection.getPledge()).thenReturn(BigInteger.ONE);
    when(projection.getFee()).thenReturn(BigInteger.ONE);
    when(projection.getMargin()).thenReturn(1D);
    poolIdPageContent.add(projection);

    Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

    when(poolHashRepository.findAllByPoolViewOrPoolNameOrPoolHash(any(), any(), any())).thenReturn(
        poolIdPage);
    List<AggregatePoolInfo> poolInfoList = new ArrayList<>();
    poolInfoList.add(
        AggregatePoolInfo.builder().blockInEpoch(1).delegatorCount(1).blockLifeTime(1).poolId(1L)
            .build());
    when(aggregatePoolInfoRepository.getAllByPoolIdIn(List.of(1L))).thenReturn(poolInfoList);
    // Call the method
    BaseFilterResponse<PoolResponse> response = delegationService.getDataForPoolTable(pageable,
        search, true);

    // Perform assertions
    assertNotNull(response);
    assertEquals(poolIdPageContent.size(), response.getData().size());
    // Add more assertions as needed
  }

  @Test
  void testGetDataForPoolDetail_thenReturn() {
    // Mocking dependencies
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

    when(poolHashRepository.getDataForPoolDetail(poolView, currentEpochNo)).thenReturn(projection);

    // Mocking fetchRewardDataService
    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(poolUpdateRepository.findOwnerAccountByPool(poolId)).thenReturn(ownerAddress);
    when(aggregatePoolInfoRepository.findByPoolId(1L)).thenReturn(AggregatePoolInfo.builder()
        .poolId(1L)
        .blockLifeTime(1)
        .delegatorCount(1)
        .blockInEpoch(1)
        .build());
    // Execute the function
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

    // Verify the interactions
    verify(epochRepository).findCurrentEpochNo();
    verify(poolHashRepository).getDataForPoolDetail(poolView, currentEpochNo);
    verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
    verify(poolUpdateRepository).findOwnerAccountByPool(poolId);

    assertEquals(poolView, result.getPoolName());
  }

  @Test
  void testGetDataForPoolDetail_thenReturnV1() {
    // Mocking dependencies
    Integer currentEpochNo = 2;
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

    when(poolHashRepository.getDataForPoolDetail(poolView, currentEpochNo)).thenReturn(projection);

    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(poolUpdateRepository.findOwnerAccountByPool(poolId)).thenReturn(ownerAddress);
    when(aggregatePoolInfoRepository.findByPoolId(1L)).thenReturn(AggregatePoolInfo.builder()
        .poolId(1L)
        .blockLifeTime(1)
        .delegatorCount(1)
        .blockInEpoch(1)
        .build());
    // Execute the function
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

    // Verify the interactions
    verify(epochRepository).findCurrentEpochNo();
    verify(poolHashRepository).getDataForPoolDetail(poolView, currentEpochNo);
    verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
    verify(poolUpdateRepository).findOwnerAccountByPool(poolId);

    assertEquals(poolView, result.getPoolName());
  }

  @Test
  void testGetDataForPoolDetail_thenReturnRos() {
    // Mocking dependencies
    Integer currentEpochNo = 1;
    Epoch epoch = new Epoch();
    epoch.setNo(currentEpochNo);
    epoch.setStartTime(Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS)));
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpochNo));

    String poolView = "1";
    Long poolId = 1L;
    PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
    when(projection.getPoolName()).thenReturn(poolView);
    when(projection.getPoolId()).thenReturn(poolId);
    when(projection.getCost()).thenReturn(BigInteger.ONE);
    when(projection.getMargin()).thenReturn(1D);
    when(projection.getPoolView()).thenReturn(poolView);

    when(poolHashRepository.getDataForPoolDetail(poolView, currentEpochNo)).thenReturn(projection);

    when(redisTemplate.opsForHash().multiGet("ACTIVATE_STAKE_null_1", List.of("1"))).thenReturn(
        List.of(BigInteger.ONE));
    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(poolUpdateRepository.findOwnerAccountByPool(poolId)).thenReturn(ownerAddress);
    when(aggregatePoolInfoRepository.findByPoolId(1L)).thenReturn(AggregatePoolInfo.builder()
        .poolId(1L)
        .blockLifeTime(1)
        .delegatorCount(1)
        .blockInEpoch(1)
        .build());
    // Execute the function
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

    // Verify the interactions
    verify(epochRepository).findCurrentEpochNo();
    verify(poolHashRepository).getDataForPoolDetail(poolView, currentEpochNo);
    verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
    verify(poolUpdateRepository).findOwnerAccountByPool(poolId);

    assertEquals(poolView, result.getPoolName());
  }

  @Test
  void testGetEpochListForPoolDetail_shouldReturnNull() {
    // Mocking the necessary data
    PageRequest pageable = PageRequest.of(0, 10);
    String poolView = "pool123";

    var response = delegationService.getEpochListForPoolDetail(pageable, poolView);

    Assertions.assertNull(response.getData());
    Assertions.assertEquals(0, response.getTotalPages());
    Assertions.assertEquals(0, response.getTotalItems());
  }

  @Test
  void testGetAnalyticsForPoolDetail() {
    // Mock repository responses
    PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
    when(projection.getPoolId()).thenReturn(1L);
    when(poolHashRepository.getDataForPoolDetail(anyString(), anyInt())).thenReturn(projection);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));

    // Mock service responses
    List<String> ownerAddress = new ArrayList<>();
    ownerAddress.add("address");
    when(poolUpdateRepository.findOwnerAccountByPool(1L)).thenReturn(ownerAddress);
    when(aggregatePoolInfoRepository.findByPoolId(1L)).thenReturn(AggregatePoolInfo.builder()
        .poolId(1L)
        .blockLifeTime(1)
        .delegatorCount(1)
        .blockInEpoch(1)
        .build());
    // Call the method
    PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail("pool_view");

    // Verify the interactions
    verify(poolHashRepository, times(1)).getDataForPoolDetail("pool_view", 1);
    verify(epochRepository, times(1)).findCurrentEpochNo();

    assertEquals(1, result.getRewardAccounts().size());
    assertEquals(1, result.getDelegators());
    assertEquals(1, result.getEpochBlock());
    assertEquals(1, result.getLifetimeBlock());
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
    when(delegationRepository.findDelegationByTxIdIn(txIdPage.getContent())).thenReturn(
        delegations);

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
    when(pds1.getMargin()).thenReturn(0.02);
    when(pds1.getPledge()).thenReturn(BigInteger.valueOf(100));
    when(pds1.getPoolId()).thenReturn(1L);
    PoolDelegationSummaryProjection pds2 = Mockito.mock(PoolDelegationSummaryProjection.class);
    when(pds2.getPoolName()).thenReturn("Pool 2");
    when(pds2.getPoolView()).thenReturn("pool2");
    when(pds2.getMargin()).thenReturn(0.03);
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
    when(delegationRepository.findDelegationPoolsSummary(Mockito.anySet()))
        .thenReturn(pools);
    when(redisTemplate.opsForHash().multiGet(any(), any())).thenReturn(
        List.of(BigInteger.valueOf(1000), BigInteger.valueOf(2000)));
    when(blockRepository.findTopDelegationByEpochBlock(1, pageable)).thenReturn(
        poolCountProjections);

    // Call the method
    List<PoolResponse> result = delegationService.findTopDelegationPool(PageRequest.of(0, 21));

    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals("pool2", result.get(0).getPoolId());
    Assertions.assertEquals(BigInteger.valueOf(200), result.get(0).getPledge());
    Assertions.assertEquals(0.03, result.get(0).getFeePercent());
    Assertions.assertEquals("pool1", result.get(1).getPoolId());
    Assertions.assertEquals("Pool 1", result.get(1).getPoolName());
    Assertions.assertEquals(0.02, result.get(1).getFeePercent());
  }

  @Test
  void testFindTopDelegationPool_shouldReturnV1() {
    PageRequest pageable = PageRequest.of(0, 20);
    // Mock data
    int currentEpoch = 2;
    PoolDelegationSummaryProjection pds = Mockito.mock(PoolDelegationSummaryProjection.class);
    when(pds.getPoolName()).thenReturn("Pool 1");
    when(pds.getPoolView()).thenReturn("pool1");
    when(pds.getMargin()).thenReturn(0.02);
    when(pds.getPledge()).thenReturn(BigInteger.valueOf(100));
    when(pds.getPoolId()).thenReturn(1L);
    List<PoolDelegationSummaryProjection> pools = List.of(pds);

    PoolCountProjection pcp = Mockito.mock(PoolCountProjection.class);
    when(pcp.getPoolId()).thenReturn(1L);
    when(pcp.getPoolView()).thenReturn("pool1");
    when(pcp.getCountValue()).thenReturn(1);
    List<PoolCountProjection> poolCountProjections = List.of(pcp);

    Set<String> poolIds = Set.of("pool1");

    when(blockRepository.findTopDelegationByEpochBlock(currentEpoch, pageable)).thenReturn(
        poolCountProjections);
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
    when(poolHashRepository.getListPoolIdIn(poolIds)).thenReturn(Set.of(1L));
    when(delegationRepository.findDelegationPoolsSummary(Set.of(1L))).thenReturn(pools);
    when(blockRepository.getCountBlockByPools(Set.of(1L))).thenReturn(poolCountProjections);
    // Call the method
    List<PoolResponse> result = delegationService.findTopDelegationPool(PageRequest.of(0, 21));

    // Verify the result
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals("pool1", result.get(0).getPoolId());
    Assertions.assertEquals("Pool 1", result.get(0).getPoolName());
    Assertions.assertEquals(BigInteger.valueOf(100), result.get(0).getPledge());
    Assertions.assertEquals(0.02, result.get(0).getFeePercent());
  }

  @Test
  void testGetAnalyticsForPoolDetail_shouldReturnNull() {
    String poolView = "poolView";

    var response = delegationService.getAnalyticsForPoolDetail(poolView);

    Assertions.assertNull(response.getEpochChart());
    Assertions.assertNull(response.getDelegatorChart());
  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturn() {
    Pageable pageable = PageRequest.of(0, 10,
        Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";
    Timestamp timestamp = new Timestamp(Instant.now().toEpochMilli());
    PoolDetailDelegatorProjection pddp = Mockito.mock(PoolDetailDelegatorProjection.class);
    when(pddp.getStakeAddressId()).thenReturn(1L);
    when(pddp.getTime()).thenReturn(timestamp);
    when(pddp.getFee()).thenReturn(BigInteger.ONE);
    when(pddp.getView()).thenReturn("view");
    Set<Long> addressIds = Set.of(1L);

    when(delegationRepository.liveDelegatorsList(poolView, pageable)).thenReturn(
        new PageImpl<>(Collections.singletonList(1L)));
    when(delegationRepository.getDelegatorsByAddress(addressIds)).thenReturn(List.of(pddp));

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);

    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1L, response.getData().get(0).getStakeAddressId());
    Assertions.assertEquals(timestamp, response.getData().get(0).getTime());
    Assertions.assertEquals(BigInteger.ONE, response.getData().get(0).getFee());
    Assertions.assertEquals("view", response.getData().get(0).getView());
  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturnV2() {
    Pageable pageable = PageRequest.of(0, 10,
        Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";
    Timestamp timestamp = new Timestamp(Instant.now().toEpochMilli());
    PoolDetailDelegatorProjection pddp = Mockito.mock(PoolDetailDelegatorProjection.class);
    when(pddp.getStakeAddressId()).thenReturn(1L);
    when(pddp.getTime()).thenReturn(timestamp);
    when(pddp.getFee()).thenReturn(BigInteger.ONE);
    when(pddp.getView()).thenReturn("view");
    Set<Long> addressIds = Set.of(1L);

    when(delegationRepository.liveDelegatorsList(poolView, pageable)).thenReturn(
        new PageImpl<>(Collections.singletonList(1L)));
    when(delegationRepository.getDelegatorsByAddress(addressIds)).thenReturn(List.of(pddp));

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);

    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1L, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1L, response.getData().get(0).getStakeAddressId());
    Assertions.assertEquals(timestamp, response.getData().get(0).getTime());
    Assertions.assertEquals(BigInteger.ONE, response.getData().get(0).getFee());

  }

  @Test
  void testGetDelegatorsForPoolDetail_thenReturnLiveDelegatorsListIsNull() {
    Pageable pageable = PageRequest.of(0, 10,
        Sort.by(Sort.Direction.DESC, Delegation_.STAKE_ADDRESS_ID));
    String poolView = "poolView";

    when(delegationRepository.liveDelegatorsList(poolView, pageable)).thenReturn(
        new PageImpl<>(List.of()));

    var response = delegationService.getDelegatorsForPoolDetail(pageable, poolView);
    Assertions.assertEquals(0, response.getTotalItems());
    Assertions.assertEquals(0, response.getTotalPages());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(0, response.getData().size());
  }
}