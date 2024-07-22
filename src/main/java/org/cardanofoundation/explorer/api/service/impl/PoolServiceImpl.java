package org.cardanofoundation.explorer.api.service.impl;

import java.util.Objects;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.pool.StakePoolsChartResponse;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.PoolService;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolServiceImpl implements PoolService {

  private final FetchRewardDataService fetchRewardDataService;

  private final PoolHashRepository poolHashRepository;

  private final EpochRepository epochRepository;

  private final PoolMapper poolMapper;

  private final RedisTemplate<String, Integer> redisTemplate;

  private final PoolUpdateRepository poolUpdateRepository;

  @Value("${application.network}")
  String network;

  @Override
  public PoolRangeValuesResponse getPoolRangeValues() {
    boolean isKoiOs = fetchRewardDataService.useKoios();
    Integer epochNo = epochRepository.findCurrentEpochNo().orElse(CommonConstant.ZERO);
    PoolRangeProjection projection;
    if (isKoiOs) {
      projection = poolHashRepository.getPoolRangeWithUsingKoi0s(epochNo);
    } else {
      projection = poolHashRepository.getPoolRangeWithoutUsingKoi0s();
    }
    return poolMapper.fromPoolRangeProjection(projection);
  }

  @Override
  public StakePoolsChartResponse getStakePoolsChart() {
    Integer currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow();

    Long registered =
        Objects.requireNonNull(
                redisTemplate.opsForValue().get(CommonConstant.REDIS_POOL_ACTIVATE + network))
            .longValue();

    Long active = poolUpdateRepository.getNumberOfActivePool(currentEpoch);

    return StakePoolsChartResponse.builder().registeredPool(registered).activePool(active).build();
  }
}
