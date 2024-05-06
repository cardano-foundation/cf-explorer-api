package org.cardanofoundation.explorer.api.projection;

public interface PoolMintBlockProjection {
  String getPoolView();

  String getPoolTicker();

  String getPoolName();

  Long getBlockNo();
}
