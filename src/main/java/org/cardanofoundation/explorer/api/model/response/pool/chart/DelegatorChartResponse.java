package org.cardanofoundation.explorer.api.model.response.pool.chart;

import java.util.List;

public class DelegatorChartResponse extends BasePoolChart<DelegatorChartList, Long> {

  public DelegatorChartResponse() {
    this.setDataByDays(List.of());
    this.setHighest(0L);
    this.setLowest(0L);
  }
}
