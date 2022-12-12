package com.cardano.explorer.model.response.pool.projection;

public interface BasePoolChartProjection<K, V> {

  K getChartKey();

  V getChartValue();
}
