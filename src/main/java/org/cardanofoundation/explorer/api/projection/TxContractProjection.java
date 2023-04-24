package org.cardanofoundation.explorer.api.projection;

import com.sotatek.cardano.common.enumeration.ScriptPurposeType;

public interface TxContractProjection {
  String getScriptHash();
  String getAddress();
  ScriptPurposeType getPurpose();
}
