package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface StakeTxProjection {
  Long getTxId();

  BigInteger getAmount();

  Long getTime();

  String getTxHash();

  Boolean getValidContract();
}
