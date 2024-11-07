package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.math.BigInteger;

import org.springframework.test.util.ReflectionTestUtils;

import org.mapstruct.factory.Mappers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.cf_explorer_aggregator.PoolAggregationRecord;
import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolServiceImpl;

@ExtendWith(MockitoExtension.class)
public class PoolServiceTest {
  @InjectMocks PoolServiceImpl poolServiceImpl;

  @Mock private PoolHashRepository poolHashRepository;
  @Mock private EpochRepository epochRepository;

  @Mock FetchRewardDataService fetchRewardDataService;
  @Mock private ExplorerAggregatorService explorerAggregatorService;

  @Spy private PoolMapper poolMapper = Mappers.getMapper(PoolMapper.class);

  @BeforeEach
  void preSetup() {
    ReflectionTestUtils.setField(poolServiceImpl, "network", "mainnet");
  }

  @Test
  void getPoolRangeValuesTest() {
    PoolRangeProjection poolRangeProjection = Mockito.mock(PoolRangeProjection.class);
    when(poolRangeProjection.getMinPledge()).thenReturn(BigInteger.ZERO);
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(poolHashRepository.getPoolRangeWithoutUsingKoi0s()).thenReturn(poolRangeProjection);
    var actual = poolServiceImpl.getPoolRangeValues();

    Assertions.assertNotNull(actual);
  }

  @Test
  void getRegiseredStakePoolsChartTest() {
    when(explorerAggregatorService.getLatestPoolAggregation())
        .thenReturn(PoolAggregationRecord.builder().activePools(30).registeredPools(10).build());
    var actual = poolServiceImpl.getStakePoolsChart();
    Assertions.assertNotNull(actual);
    Assertions.assertEquals(10, actual.getRegisteredPool());
    Assertions.assertEquals(30, actual.getActivePool());
  }
}
