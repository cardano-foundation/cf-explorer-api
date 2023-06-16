package org.cardanofoundation.explorer.api.projection;

public interface DelegationProjection {
  Long getTxId();
  String getPoolView();
  String getPoolName();
  String getTickerName();
  String getStakeAddress();
}
