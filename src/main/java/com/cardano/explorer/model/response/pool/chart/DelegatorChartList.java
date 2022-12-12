package com.cardano.explorer.model.response.pool.chart;

import com.cardano.explorer.model.response.pool.projection.DelegatorChartProjection;

public class DelegatorChartList extends BaseChartList<Long, Long> {

  public DelegatorChartList(DelegatorChartProjection delegatorChart) {
    this.setXChart(delegatorChart.getChartKey());
    this.setYChart(delegatorChart.getChartValue());
  }
}
