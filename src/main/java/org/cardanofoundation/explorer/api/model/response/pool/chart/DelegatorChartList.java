package org.cardanofoundation.explorer.api.model.response.pool.chart;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.pool.projection.DelegatorChartProjection;

@Getter
@Setter
public class DelegatorChartList {

  private Integer epochNo;

  private Long numberDelegator;

  public DelegatorChartList(DelegatorChartProjection delegatorChart) {
    this.epochNo = delegatorChart.getChartKey();
    this.numberDelegator = delegatorChart.getChartValue();
  }
}
