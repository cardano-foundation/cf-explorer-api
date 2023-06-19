package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;
import java.time.LocalDateTime;

public interface TxIOProjection {
  Long getId();
  Long getBlockNo();
  String getFromAddress();
  String getToAddress();
  String getHash();
  BigInteger getAmount();
  Boolean getValidContract();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  Integer getSlot();
  LocalDateTime getTime();
}
