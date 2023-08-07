package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.TxIOProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class TxIOProjectionImpl implements TxIOProjection {
  Long id;
  Long blockNo;
  String fromAddress;
  String toAddress;
  String hash;
  BigInteger amount;
  Boolean validContract;
  Integer epochNo;
  Integer epochSlotNo;
  Integer slot;
  LocalDateTime time;

  @Override
  public Long getId() {
    return this.id;
  }

  @Override
  public Long getBlockNo() {
    return this.blockNo;
  }

  @Override
  public String getFromAddress() {
    return this.fromAddress;
  }

  @Override
  public String getToAddress() {
    return this.toAddress;
  }

  @Override
  public String getHash() {
    return this.hash;
  }

  @Override
  public BigInteger getAmount() {
    return this.amount;
  }

  @Override
  public Boolean getValidContract() {
    return this.validContract;
  }

  @Override
  public Integer getEpochNo() {
    return this.epochNo;
  }

  @Override
  public Integer getEpochSlotNo() {
    return this.epochSlotNo;
  }

  @Override
  public Integer getSlot() {
    return this.slot;
  }

  @Override
  public LocalDateTime getTime() {
    return this.time;
  }
}
