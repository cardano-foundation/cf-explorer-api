package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface TxGraphProjection {

  BigInteger getTime();

  BigInteger getSimpleTransactions();

  BigInteger getSmartContract();

  BigInteger getMetadata();
}
