package org.cardanofoundation.explorer.api.model.response.script.projection;

import java.sql.Timestamp;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

public interface SmartContractTxProjection {
  String getHash();
  Timestamp getTime();
  Long getBlockNo();
  Integer getEpochNo();
  Integer getEpochSlotNo();
  Integer getAbsoluteSlot();
  Long getTxId();
  ScriptPurposeType getScriptPurposeType();
}
