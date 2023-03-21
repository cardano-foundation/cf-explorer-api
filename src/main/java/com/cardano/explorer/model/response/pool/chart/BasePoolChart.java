package com.cardano.explorer.model.response.pool.chart;

import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public abstract class BasePoolChart<K, T> {

  private T highest;

  private T lowest;

  private List<K> dataByDays;
}
