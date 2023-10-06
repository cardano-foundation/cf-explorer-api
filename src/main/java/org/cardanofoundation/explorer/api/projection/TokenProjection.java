package org.cardanofoundation.explorer.api.projection;


import java.math.BigInteger;
import java.sql.Timestamp;

public interface TokenProjection {

  Long getId();

  String getPolicy();

  String getName();

  String getNameView();

  String getFingerprint();

  Long getTxCount();

  BigInteger getSupply();

  BigInteger getTotalVolume();

  Timestamp getTime();
  
  Integer getNameViewLength();

}
