package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface MintProjection {

  String getName();

  String getPolicy();

  BigInteger getAssetQuantity();

  String getFingerprint();

  String getUrl();

  String getTicker();

  Integer getDecimals();

  String getLogo();

  String getDescription();
}
