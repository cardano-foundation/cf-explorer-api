package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

public interface TxContractProjection {
  String getScriptHash();
  String getAddress();
  ScriptPurposeType getPurpose();
}
