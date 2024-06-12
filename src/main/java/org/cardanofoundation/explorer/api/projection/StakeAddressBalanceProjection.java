package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface StakeAddressBalanceProjection {
  String getAddress();

  BigInteger getBalance();
}
