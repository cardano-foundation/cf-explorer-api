package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class TxContractProjectionImpl implements TxContractProjection {
  String scriptHash;
  String address;
  ScriptPurposeType purpose;
  byte[] redeemerBytes;
  BigInteger redeemerMem;
  BigInteger redeemerSteps;
  String datumHashIn;
  byte[] datumBytesIn;
  byte[] scriptBytes;
  String datumHashOut;
  byte[] datumBytesOut;
  Long txOutId;
  String stakeAddress;

  @Override
  public String getScriptHash() {
    return this.scriptHash;
  }

  @Override
  public String getAddress() {
    return this.address;
  }

  @Override
  public ScriptPurposeType getPurpose() {
    return this.purpose;
  }

  @Override
  public byte[] getRedeemerBytes() {
    return this.redeemerBytes;
  }

  @Override
  public BigInteger getRedeemerMem() {
    return this.redeemerMem;
  }

  @Override
  public BigInteger getRedeemerSteps() {
    return this.redeemerSteps;
  }

  @Override
  public String getDatumHashIn() {
    return this.datumHashIn;
  }

  @Override
  public byte[] getDatumBytesIn() {
    return this.datumBytesIn;
  }

  @Override
  public byte[] getScriptBytes() {
    return this.scriptBytes;
  }

  @Override
  public String getDatumHashOut() {
    return this.datumHashOut;
  }

  @Override
  public byte[] getDatumBytesOut() {
    return this.datumBytesOut;
  }

  @Override
  public Long getTxOutId() {
    return this.txOutId;
  }

  @Override
  public String getStakeAddress() {
    return this.stakeAddress;
  }
}
