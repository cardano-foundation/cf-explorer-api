package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface TxProjection {
  Long getId();
  Long getBlockId();
  Long getBlockIndex();
  Long getDeposit();
  BigInteger getFee();
  String getHash();
  BigInteger getInvalidBefore();
  BigInteger getInvalidHereafter();
  BigInteger getOutSum();
  Integer getScriptSize();
  Integer getSize();
  Boolean getValidContract();
  Long getAddressId();
}
