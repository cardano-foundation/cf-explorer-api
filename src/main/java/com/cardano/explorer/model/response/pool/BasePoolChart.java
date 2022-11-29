package com.cardano.explorer.model.response.pool;

import java.util.Map;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public abstract class BasePoolChart<K, T> {

  private T highest;

  private T lowest;

  private Map<K, T> dataByDays;
}
