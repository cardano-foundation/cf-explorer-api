package com.cardano.explorer.projection;

import java.math.BigInteger;
import java.time.LocalDateTime;

public interface TxIOProjection {
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
