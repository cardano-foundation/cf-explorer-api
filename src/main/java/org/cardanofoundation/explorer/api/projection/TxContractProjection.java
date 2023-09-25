package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

public interface TxContractProjection {
  String getScriptHash();
  String getAddress();
  ScriptPurposeType getPurpose();
  byte[] getRedeemerBytes();
  BigInteger getRedeemerMem();
  BigInteger getRedeemerSteps();
  String getDatumHashIn();
  byte[] getDatumBytesIn();
  byte[] getScriptBytes();
  String getDatumHashOut();
  byte[] getDatumBytesOut();
  Long getTxOutId();
  String getStakeAddress();
  String getUtxoHash();
  Integer getUtxoIndex();
}
