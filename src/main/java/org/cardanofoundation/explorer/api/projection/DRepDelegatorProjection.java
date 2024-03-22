package org.cardanofoundation.explorer.api.projection;

public interface DRepDelegatorProjection {
  String getStakeAddress();

  String getTxHash();

  Long getBlockTime();

  Integer getFee();
}
