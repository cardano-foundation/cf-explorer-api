package com.cardano.explorer.model.response.pool;

import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailAnalyticsResponse implements Serializable {

  private EpochChartResponse epochChart;

  private DelegatorChartResponse delegatorChart;
}
