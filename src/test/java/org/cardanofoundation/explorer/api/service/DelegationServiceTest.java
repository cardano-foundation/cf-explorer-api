package org.cardanofoundation.explorer.api.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.mapper.BlockMapper;
import org.cardanofoundation.explorer.api.model.response.*;
import org.cardanofoundation.explorer.api.model.response.address.AddressAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.pool.DelegationHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailEpochResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolDetailHeaderResponse;
import org.cardanofoundation.explorer.api.model.response.pool.PoolResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailUpdateProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.projection.DelegationProjection;
import org.cardanofoundation.explorer.api.projection.PoolDelegationSummaryProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.service.impl.BlockServiceImpl;
import org.cardanofoundation.explorer.api.service.impl.DelegationServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Answers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.*;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

@ExtendWith(MockitoExtension.class)
public class DelegationServiceTest {

    @Mock
    private DelegationRepository delegationRepository;

    @Mock
    private BlockRepository blockRepository;

    @Mock
    private EpochRepository epochRepository;

    @Mock
    private EpochStakeRepository epochStakeRepository;

    @Mock
    private PoolHashRepository poolHashRepository;

    @Mock
    private PoolUpdateRepository poolUpdateRepository;

    @Mock
    private RewardRepository rewardRepository;

    @Mock
    private PoolInfoRepository poolInfoRepository;

    @Mock
    private PoolHistoryRepository poolHistoryRepository;

    @Mock(answer = Answers.RETURNS_DEEP_STUBS)
    private RedisTemplate<String, Object> redisTemplate;

    @Mock
    private FetchRewardDataService fetchRewardDataService;

    @Mock
    private StakeAddressRepository stakeAddressRepository;

    @Mock
    private TxRepository txRepository;

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
    public void testGetDataForDelegationHeader() {
        // Mock dependencies
        Epoch epoch = new Epoch();
        epoch.setNo(1);
        epoch.setStartTime(Timestamp.from(Instant.now()));
        when(epochRepository.findByCurrentEpochNo()).thenReturn(Optional.of(epoch));
        when(fetchRewardDataService.checkAdaPots(anyInt())).thenReturn(true);
        when(fetchRewardDataService.isKoiOs()).thenReturn(true);
        when(poolInfoRepository.getTotalLiveStake(anyInt())).thenReturn(BigInteger.TEN);
        when(delegationRepository.totalLiveDelegatorsCount()).thenReturn(5);

        // Execute the method
        DelegationHeaderResponse response = delegationService.getDataForDelegationHeader();

        // Verify the results
        assertEquals(1, response.getEpochNo());
        assertEquals(0, response.getEpochSlotNo()); // Update this based on the actual calculation
        assertEquals(BigInteger.TEN, response.getLiveStake());
        assertEquals(5, response.getDelegators());

        // Verify interactions with dependencies
        verify(epochRepository).findByCurrentEpochNo();
        verify(fetchRewardDataService).checkAdaPots(1);
        verify(fetchRewardDataService).isKoiOs();
        verify(poolInfoRepository).getTotalLiveStake(1);
        verify(delegationRepository).totalLiveDelegatorsCount();
    }

    @Test
    public void testGetDataForPoolTable_withSearch() {
        // Mocked input data
        Pageable pageable = PageRequest.of(0, 10);
        String search = "example";

        // Mocked repository responses
        List<PoolListProjection> poolIdPageContent = new ArrayList<>();
        // Add mock data to poolIdPageContent

        Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

        when(poolHashRepository.findAllByPoolViewAndPoolName(search, pageable)).thenReturn(poolIdPage);

        // Call the method
        BaseFilterResponse<PoolResponse> response = delegationService.getDataForPoolTable(pageable, search);

        // Perform assertions
        assertNotNull(response);
        assertEquals(poolIdPageContent.size(), response.getData().size());
        // Add more assertions as needed
    }

    @Test
    public void testGetDataForPoolTable_withoutSearch() {
        // Mocked input data
        Pageable pageable = PageRequest.of(0, 10);
        String search = null;

        // Mocked repository responses
        List<PoolListProjection> poolIdPageContent = new ArrayList<>();
        // Add mock data to poolIdPageContent

        Page<PoolListProjection> poolIdPage = new PageImpl<>(poolIdPageContent, pageable, 20L);

        when(poolHashRepository.findAllByPoolViewAndPoolName(null, pageable)).thenReturn(poolIdPage);
        // Call the method
        BaseFilterResponse<PoolResponse> response = delegationService.getDataForPoolTable(pageable, search);

        // Perform assertions
        assertNotNull(response);
        assertEquals(poolIdPageContent.size(), response.getData().size());
        // Add more assertions as needed
    }

    @Test
    public void testGetDataForPoolDetail() {
        // Mocking dependencies
        Integer currentEpochNo = 1;
        Epoch epoch = new Epoch();
        epoch.setNo(currentEpochNo);
        epoch.setStartTime(Timestamp.from(Instant.now().minus(5, ChronoUnit.DAYS)));
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpochNo));
//        when(epochRepository.findByCurrentEpochNo()).thenReturn(Optional.of(epoch));

        String poolView = "poolView";
        Long poolId = 1L;
        BigInteger reserves = BigInteger.valueOf(1000);
        Integer paramK = 2;
        PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
        when(projection.getPoolName()).thenReturn(poolView);
        when(projection.getPoolId()).thenReturn(poolId);
        when(projection.getReserves()).thenReturn(reserves);
        when(projection.getParamK()).thenReturn(paramK);

        when(poolHashRepository.getDataForPoolDetail(poolView, currentEpochNo)).thenReturn(projection);

        // Mocking fetchRewardDataService
        when(fetchRewardDataService.checkAdaPots(currentEpochNo)).thenReturn(true);

        // Execute the function
        PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail(poolView);

        // Verify the interactions
        verify(epochRepository).findCurrentEpochNo();
        verify(poolHashRepository).getDataForPoolDetail(poolView, currentEpochNo);
        verify(rewardRepository).getPoolRewardByPool(poolId);
        verify(poolUpdateRepository).getCreatedTimeOfPool(poolId);
        verify(poolUpdateRepository).findOwnerAccountByPool(poolId);
        verify(delegationRepository).liveDelegatorsCount(poolView);
        verify(blockRepository).getCountBlockByPoolAndCurrentEpoch(poolId);
        verify(blockRepository).getCountBlockByPoolAndCurrentEpoch(poolId);

        assertEquals(poolView, result.getPoolName());
    }

    @Test
    public void testGetEpochListForPoolDetail() {
        // Mocking the necessary data
        Pageable pageable = PageRequest.of(0, 10);
        String poolView = "pool123";
        int currentEpoch = 10;
        Epoch.builder().build();

        List<Epoch> epochs = Arrays.asList(
                Epoch.builder().id(9L).endTime(Timestamp.valueOf("2023-06-01 00:00:00")).build(),
                Epoch.builder().id(10L).endTime(Timestamp.valueOf("2023-06-05 00:00:00")).build(),
                Epoch.builder().id(11L).endTime(Timestamp.valueOf("2023-06-09 00:00:00")).build()
        );

        // Stubbing the mocked methods
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(currentEpoch));
        when(poolHashRepository.findByView(poolView)).thenReturn(Optional.of(PoolHash.builder().build()));
//        when(epochRepository.findByEpochNoGreaterThanEqual(currentEpoch - 2)).thenReturn(epochs);

        // Calling the method to be tested
        BaseFilterResponse<PoolDetailEpochResponse> epochList = delegationService.getEpochListForPoolDetail(pageable, poolView);

        // Assertions
        assert epochList.getTotalPages() == 3;
        assert epochList.getData().get(0).getEpoch() == 9;
        assert epochList.getData().get(1).getEpoch() == 10;
        assert epochList.getData().get(2).getEpoch() == 11;


        verify(epochRepository).findCurrentEpochNo();
//        verify(epochRepository).findByEpochNoGreaterThanEqual(currentEpoch - 2);
        verifyNoMoreInteractions(epochRepository);
    }

    @Test
    public void testGetAnalyticsForPoolDetail() {
        // Mock repository responses
        PoolDetailUpdateProjection projection = Mockito.mock(PoolDetailUpdateProjection.class);
        when(projection.getPoolId()).thenReturn(1L);
        when(projection.getReserves()).thenReturn(BigInteger.valueOf(100));
        when(projection.getParamK()).thenReturn(1);
        when(poolHashRepository.getDataForPoolDetail(anyString(), anyInt())).thenReturn(projection);
        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));

        // Mock service responses
        when(fetchRewardDataService.checkAdaPots(anyInt())).thenReturn(true);
        when(fetchRewardDataService.isKoiOs()).thenReturn(false);

        // Call the method
        PoolDetailHeaderResponse result = delegationService.getDataForPoolDetail("pool_view");

        // Verify the interactions
        verify(poolHashRepository, times(1)).getDataForPoolDetail("pool_view", 1);
        verify(epochRepository, times(1)).findCurrentEpochNo();
        verify(fetchRewardDataService, times(1)).checkAdaPots(1);
        verify(fetchRewardDataService, times(1)).isKoiOs();

        assertEquals(1, result.getRewardAccounts().size());
        // Perform assertions
    }

    @Test
    public void testGetDelegatorsForPoolDetail() {
        // Mock data
        Pageable pageable = Pageable.unpaged();
        Long txId1 = 1L;
        Long txId2 = 2L;
        TxIOProjection tx1 = Mockito.mock(TxIOProjection.class);
        when(tx1.getId()).thenReturn(txId1);
        when(tx1.getHash()).thenReturn("hash1");
        when(tx1.getSlot()).thenReturn(1);
        when(tx1.getEpochNo()).thenReturn(1);
        when(tx1.getEpochSlotNo()).thenReturn(1);
        when(tx1.getBlockNo()).thenReturn(1L);
        when(tx1.getFromAddress()).thenReturn("stakeAddress1");
        when(tx1.getToAddress()).thenReturn("stakeAddress1");
        TxIOProjection tx2 = Mockito.mock(TxIOProjection.class);
        when(tx1.getId()).thenReturn(txId2);
        when(tx1.getHash()).thenReturn("hash2");
        when(tx1.getSlot()).thenReturn(2);
        when(tx1.getEpochNo()).thenReturn(2);
        when(tx1.getEpochSlotNo()).thenReturn(2);
        when(tx1.getBlockNo()).thenReturn(2L);
        when(tx1.getFromAddress()).thenReturn("stakeAddress2");
        when(tx1.getToAddress()).thenReturn("stakeAddress2");
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
        List<DelegationProjection> delegations = Arrays.asList(d1,d2);

        // Mock repository methods
        Page<Long> txIdPage = new PageImpl<>(List.of(txId1, txId2));
        when(delegationRepository.findAllDelegations(pageable)).thenReturn(txIdPage);
        when(txRepository.findTxIn(txIdPage.getContent())).thenReturn(txs);
        when(delegationRepository.findDelegationByTxIdIn(txIdPage.getContent())).thenReturn(delegations);

        // Call the service method
        BaseFilterResponse<DelegationResponse> response = delegationService.getDelegations(pageable);

        // Assertions
        assertEquals(txIdPage, response.getData());
        assertEquals(2, response.getData().size());
        assertEquals(tx1.getHash(), response.getData().get(0).getTxHash());
        assertEquals(tx2.getHash(), response.getData().get(1).getTxHash());
        // Add more assertions as needed
    }

    @Test
    public void testFindTopDelegationPool() {
        // Mock data
        PoolDelegationSummaryProjection pds1 = Mockito.mock(PoolDelegationSummaryProjection.class);
        when(pds1.getPoolName()).thenReturn("Pool 1");
        when(pds1.getPoolView()).thenReturn("pool1");
        when(pds1.getMargin()).thenReturn(0.02);
        when(pds1.getPledge()).thenReturn(BigInteger.valueOf(100));
        PoolDelegationSummaryProjection pds2 = Mockito.mock(PoolDelegationSummaryProjection.class);
        when(pds2.getPoolName()).thenReturn("Pool 2");
        when(pds2.getPoolView()).thenReturn("pool2");
        when(pds2.getMargin()).thenReturn(0.03);
        when(pds2.getPledge()).thenReturn(BigInteger.valueOf(200));
        List<PoolDelegationSummaryProjection> pools = new ArrayList<>();
        pools.add(pds1);
        pools.add(pds2);

        when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(1));
        when(delegationRepository.findDelegationPoolsSummary(Mockito.anySet()))
                .thenReturn(pools);
        when(poolHashRepository.findAllPoolView()).thenReturn(List.of("pool1", "pool2"));
        when(redisTemplate.opsForHash().multiGet(any(), any())).thenReturn(List.of(BigInteger.valueOf(1000), BigInteger.valueOf(2000)));


        // Call the method
        List<PoolResponse> result = delegationService.findTopDelegationPool(PageRequest.of(0, 20));

        // Verify the result
        assert result.size() == 2;
        assert result.get(0).getPoolId().equals("pool2");
        assert result.get(0).getPoolName().equals("Pool 2");
        assert result.get(0).getPledge().equals(BigInteger.valueOf(200));
        assert result.get(0).getFeePercent() == 0.03;
        assert result.get(0).getReserves().equals(BigInteger.valueOf(2000));
        assert result.get(1).getPoolId().equals("pool1");
        assert result.get(1).getPoolName().equals("Pool 1");
        assert result.get(1).getPledge().equals(BigInteger.valueOf(100));
        assert result.get(1).getFeePercent() == 0.02;
        assert result.get(1).getReserves().equals(BigInteger.valueOf(1000));
    }



}