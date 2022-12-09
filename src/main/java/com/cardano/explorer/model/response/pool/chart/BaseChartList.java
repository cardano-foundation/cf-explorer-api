package com.cardano.explorer.model.response.pool.chart;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public abstract class BaseChartList<K, V> {

  private K xChart;

  private V yChart;
}
