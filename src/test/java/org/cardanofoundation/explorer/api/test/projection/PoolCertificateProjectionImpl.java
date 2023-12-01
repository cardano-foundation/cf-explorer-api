package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCertificateProjection;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class PoolCertificateProjectionImpl implements PoolCertificateProjection {

  Long txId;
  String txHash;
  Integer txEpochNo;
  Integer certEpochNo;
  Integer certIndex;
  Long poolRetireId;
  Long poolUpdateId;
  Timestamp blockTime;
  Long blockNo;
  Integer epochSlotNo;
  Integer slotNo;

  @Override
  public Long getTxId() {
    return this.txId;
  }

  @Override
  public String getTxHash() {
    return this.txHash;
  }

  @Override
  public Integer getTxEpochNo() {
    return this.txEpochNo;
  }

  @Override
  public Integer getCertEpochNo() {
    return this.certEpochNo;
  }

  @Override
  public Integer getCertIndex() {
    return this.certIndex;
  }

  @Override
  public Long getPoolRetireId() {
    return this.poolRetireId;
  }

  @Override
  public Long getPoolUpdateId() {
    return this.poolUpdateId;
  }

  @Override
  public Timestamp getBlockTime() {
    return this.blockTime;
  }

  @Override
  public Long getBlockNo() {
    return this.blockNo;
  }

  @Override
  public Integer getEpochSlotNo() {
    return this.epochSlotNo;
  }

  @Override
  public Integer getSlotNo() {
    return this.slotNo;
  }
}
