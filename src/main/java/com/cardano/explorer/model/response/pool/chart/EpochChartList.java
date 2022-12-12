package com.cardano.explorer.model.response.pool.chart;

import com.cardano.explorer.model.response.pool.projection.EpochChartProjection;
import java.math.BigDecimal;


public class EpochChartList extends BaseChartList<Integer, BigDecimal> {

  public EpochChartList(EpochChartProjection epochChart) {
    this.setXChart(epochChart.getChartKey());
    this.setYChart(epochChart.getChartValue());
  }
}
