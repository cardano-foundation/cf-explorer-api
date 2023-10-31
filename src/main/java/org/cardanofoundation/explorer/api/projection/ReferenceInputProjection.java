package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface ReferenceInputProjection {
  Long getTxOutId();
  String getAddress();
  String getTxHash();
  BigInteger getValue();
  Integer getIndex();
  String getScriptHash();
  byte[] getScriptBytes();
  String getDatumHash();
  byte[] getDatumBytes();
}
