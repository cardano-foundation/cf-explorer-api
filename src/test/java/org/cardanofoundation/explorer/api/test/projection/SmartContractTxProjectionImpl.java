package org.cardanofoundation.explorer.api.test.projection;

import java.sql.Timestamp;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class SmartContractTxProjectionImpl implements SmartContractTxProjection {
  String hash;
  Timestamp getTime;
  Long blockNo;
  Integer epochNo;
  Integer epochSlotNo;
  Integer absoluteSlot;
  Long txId;
  ScriptPurposeType scriptPurposeType;

  @Override
  public String getHash() {
    return this.hash;
  }

  @Override
  public Timestamp getTime() {
    return this.getTime;
  }

  @Override
  public Long getBlockNo() {
    return this.blockNo;
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
  public Integer getAbsoluteSlot() {
    return this.absoluteSlot;
  }

  @Override
  public Long getTxId() {
    return this.txId;
  }

  @Override
  public ScriptPurposeType getScriptPurposeType() {
    return this.scriptPurposeType;
  }
}
