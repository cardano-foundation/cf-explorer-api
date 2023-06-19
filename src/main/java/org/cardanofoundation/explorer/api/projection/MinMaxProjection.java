package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface MinMaxProjection {

  BigInteger getMinVal();

  BigInteger getMaxVal();

  Long getMaxTxId();
}
