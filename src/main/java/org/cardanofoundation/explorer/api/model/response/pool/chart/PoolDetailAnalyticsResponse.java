package org.cardanofoundation.explorer.api.model.response.pool.chart;

import java.io.Serializable;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PoolDetailAnalyticsResponse implements Serializable {

  private EpochChartResponse epochChart;

  private DelegatorChartResponse delegatorChart;
}
