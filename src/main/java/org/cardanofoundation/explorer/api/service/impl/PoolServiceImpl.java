package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.mapper.PoolMapper;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.projection.PoolRangeProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
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
}
