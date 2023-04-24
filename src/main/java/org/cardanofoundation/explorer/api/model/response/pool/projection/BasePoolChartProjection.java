package org.cardanofoundation.explorer.api.model.response.pool.projection;

public interface BasePoolChartProjection<K, V> {

  K getChartKey();

  V getChartValue();
}
