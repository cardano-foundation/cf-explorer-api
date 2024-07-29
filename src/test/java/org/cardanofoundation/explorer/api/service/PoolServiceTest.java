package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.when;

import java.math.BigInteger;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
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

import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolServiceImpl;

@ExtendWith(MockitoExtension.class)
public class PoolServiceTest {
  @InjectMocks PoolServiceImpl poolServiceImpl;

  @Mock FetchRewardDataService fetchRewardDataService;

  @Mock private PoolHashRepository poolHashRepository;

  @Mock private EpochRepository epochRepository;
  @Mock private RedisTemplate<String, Integer> redisTemplate;
  @Mock private PoolUpdateRepository poolUpdateRepository;
  @Mock private ValueOperations valueOperations;
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
    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    when(valueOperations.get(any())).thenReturn(10);
    when(epochRepository.findCurrentEpochNo()).thenReturn(java.util.Optional.of(20));
    when(poolUpdateRepository.getNumberOfActivePool(anyInt())).thenReturn(30L);

    var actual = poolServiceImpl.getStakePoolsChart();
    Assertions.assertNotNull(actual);
    Assertions.assertEquals(10, actual.getRegisteredPool());
    Assertions.assertEquals(30, actual.getActivePool());
  }
}
