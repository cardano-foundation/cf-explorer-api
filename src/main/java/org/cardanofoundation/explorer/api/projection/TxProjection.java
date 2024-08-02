package org.cardanofoundation.explorer.api.projection;

public interface TxProjection {
  String getTxHash();

  Long getBlockTime();

  Long getSlot();
}
