package com.cardano.explorer.model.response.pool.chart;

import com.cardano.explorer.model.response.pool.custom.EpochDataChart;
import java.math.BigDecimal;
import java.util.Date;


public class EpochChartList extends BaseChartList<Date, BigDecimal> {

  public EpochChartList(EpochDataChart epochDataChart) {
    this.setTimeChart(epochDataChart.getTime());
    this.setDataChart(epochDataChart.getChartValue());
  }
}
