package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;
import java.sql.Timestamp;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class PoolDeRegistrationProjectionImpl implements PoolDeRegistrationProjection {
  Boolean refundFlag;
  BigInteger fee;
  String txHash;
  Long txId;
  String poolId;
  Integer retiringEpoch;
  Timestamp time;

  @Override
  public Boolean getRefundFlag() {
    return this.refundFlag;
  }

  @Override
  public BigInteger getFee() {
    return this.fee;
  }

  @Override
  public String getTxHash() {
    return this.txHash;
  }

  @Override
  public Long getTxId() {
    return this.txId;
  }

  @Override
  public String getPoolId() {
    return this.poolId;
  }

  @Override
  public Integer getRetiringEpoch() {
    return this.retiringEpoch;
  }

  @Override
  public Timestamp getTime() {
    return this.time;
  }
}
