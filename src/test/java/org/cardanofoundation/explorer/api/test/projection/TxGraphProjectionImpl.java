package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;

import lombok.Builder;

import org.cardanofoundation.explorer.api.projection.TxGraphProjection;

@Builder
public class TxGraphProjectionImpl implements TxGraphProjection {
  private BigInteger time;
  private BigInteger simpleTransactions;
  private BigInteger smartContract;
  private BigInteger metadata;

  @Override
  public BigInteger getTime() {
    return this.time;
  }

  @Override
  public BigInteger getSimpleTransactions() {
    return this.simpleTransactions;
  }

  @Override
  public BigInteger getSmartContract() {
    return this.smartContract;
  }

  @Override
  public BigInteger getMetadata() {
    return this.metadata;
  }
}
