package org.cardanofoundation.explorer.api.model.response.pool.chart;

import java.io.Serializable;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PoolDetailAnalyticsResponse implements Serializable {

  private EpochChartResponse epochChart;

  private DelegatorChartResponse delegatorChart;
}
