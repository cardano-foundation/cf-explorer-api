package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.model.response.pool.PoolRangeValuesResponse;
import org.cardanofoundation.explorer.api.model.response.pool.RegisteredStakePoolsChartResponse;

public interface PoolService {
  PoolRangeValuesResponse getPoolRangeValues();

  RegisteredStakePoolsChartResponse getRegisteredStakePoolsChart();
}
