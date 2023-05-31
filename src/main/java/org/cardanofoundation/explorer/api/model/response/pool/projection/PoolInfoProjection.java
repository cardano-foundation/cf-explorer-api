package org.cardanofoundation.explorer.api.model.response.pool.projection;

public interface PoolInfoProjection {

  Long getId();

  String getPoolId();

  String getPoolView();

  String getPoolName();

  String getRewardAccount();
}
