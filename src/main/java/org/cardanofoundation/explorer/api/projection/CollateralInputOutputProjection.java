package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface CollateralInputOutputProjection {
  String getAddress();
  String getTxHash();
  BigInteger getValue();
}
