package com.cardano.explorer.model.response.pool.custom;

public interface BaseDataPoolChart<K, V> {

  K getTime();

  V getChartValue();
}
