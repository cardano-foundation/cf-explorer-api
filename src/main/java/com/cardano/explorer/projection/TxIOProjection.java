package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface TxIOProjection {
  Long getBlockNo();
  String getFromAddress();
  String getToAddress();
  String getHash();
  BigDecimal getAmount();
}
