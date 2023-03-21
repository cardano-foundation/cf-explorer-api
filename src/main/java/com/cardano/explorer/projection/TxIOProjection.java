package com.cardano.explorer.projection;

import java.math.BigInteger;

public interface TxIOProjection {
  Long getBlockNo();
  String getFromAddress();
  String getToAddress();
  String getHash();
  BigInteger getAmount();
}
