package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import org.cardanofoundation.cf_explorer_aggregator.PoolAggregationRecord;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.pool.StakePoolsChartResponse;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.service.ExplorerAggregatorService;
import org.cardanofoundation.explorer.api.service.FetchRewardDataService;
import org.cardanofoundation.explorer.api.service.PoolService;

@Service
@RequiredArgsConstructor
@Log4j2
public class PoolServiceImpl implements PoolService {

  private final PoolHashRepository poolHashRepository;
  private final EpochRepository epochRepository;

  private final FetchRewardDataService fetchRewardDataService;
  private final ExplorerAggregatorService explorerAggregatorService;

  private final PoolMapper poolMapper;

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
    PoolAggregationRecord latestPoolAggregation =
        explorerAggregatorService.getLatestPoolAggregation();
    return StakePoolsChartResponse.builder()
        .registeredPool(latestPoolAggregation.getRegisteredPools().longValue())
        .activePool(latestPoolAggregation.getActivePools().longValue())
        .build();
  }
}
