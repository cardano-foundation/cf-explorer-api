package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.pool.StakePoolsChartResponse;

public interface PoolService {
  PoolRangeValuesResponse getPoolRangeValues();

  StakePoolsChartResponse getStakePoolsChart();
}
