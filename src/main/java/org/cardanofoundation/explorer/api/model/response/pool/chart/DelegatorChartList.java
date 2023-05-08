package org.cardanofoundation.explorer.api.model.response.pool.chart;

import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DelegatorChartList {

  private Long epochNo;

  private Long numberDelegator;

  public DelegatorChartList(DelegatorChartProjection delegatorChart) {
    this.epochNo = delegatorChart.getChartKey();
    this.numberDelegator = delegatorChart.getChartValue();
  }
}
