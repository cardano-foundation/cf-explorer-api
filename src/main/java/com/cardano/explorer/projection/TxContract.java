package com.cardano.explorer.projection;

import com.sotatek.cardano.common.enumeration.ScriptPurposeType;

public interface TxContract {
  String getScriptHash();
  String getAddress();
  ScriptPurposeType getPurpose();
}
