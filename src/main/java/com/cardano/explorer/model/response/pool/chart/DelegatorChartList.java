package com.cardano.explorer.model.response.pool.chart;

import com.cardano.explorer.model.response.pool.custom.DelegatorDataChart;
import java.util.Date;

public class DelegatorChartList extends BaseChartList<Date, Long> {

  public DelegatorChartList(DelegatorDataChart delegatorDataChart) {
    this.setTimeChart(delegatorDataChart.getTime());
    this.setDataChart(delegatorDataChart.getChartValue());
  }
}
