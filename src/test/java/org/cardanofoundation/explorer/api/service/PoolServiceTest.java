package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.math.BigInteger;

import org.mapstruct.factory.Mappers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.service.impl.PoolServiceImpl;

@ExtendWith(MockitoExtension.class)
public class PoolServiceTest {
  @InjectMocks PoolServiceImpl poolServiceImpl;

  @Mock FetchRewardDataService fetchRewardDataService;

  @Mock private PoolHashRepository poolHashRepository;

  @Mock private EpochRepository epochRepository;
  @Spy private PoolMapper poolMapper = Mappers.getMapper(PoolMapper.class);

  @Test
  void getPoolRangeValuesTest() {
    PoolRangeProjection poolRangeProjection = Mockito.mock(PoolRangeProjection.class);
    when(poolRangeProjection.getMinPledge()).thenReturn(BigInteger.ZERO);
    when(fetchRewardDataService.useKoios()).thenReturn(false);
    when(poolHashRepository.getPoolRangeWithoutUsingKoi0s()).thenReturn(poolRangeProjection);
    var actual = poolServiceImpl.getPoolRangeValues();

    Assertions.assertNotNull(actual);
  }
}
