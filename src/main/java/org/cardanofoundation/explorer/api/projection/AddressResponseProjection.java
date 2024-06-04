package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface AddressResponseProjection {
  String getAddress();

  Long getTxCount();

  BigInteger getBalance();
}
