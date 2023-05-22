package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import lombok.Builder;

@Builder
public class TxGraphProjectionImp implements TxGraphProjection {

  private BigInteger time;
  private BigInteger simpleTransactions;
  private BigInteger smartContract;
  private BigInteger metadata;

  @Override
  public BigInteger getTime() {
    return time;
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

  public void setSimpleTransactions(BigInteger simpleTransactions) {
    this.simpleTransactions = simpleTransactions;
  }

  public void setSmartContract(BigInteger smartContract) {
    this.smartContract = smartContract;
  }

  public void setMetadata(BigInteger metadata) {
    this.metadata = metadata;
  }
}
