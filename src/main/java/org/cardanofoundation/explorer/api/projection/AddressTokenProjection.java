package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface AddressTokenProjection {

  BigInteger getQuantity();
  String getFingerprint();
  String getTokenName();
  String getAddress();
  Long getAddressId();
  String getPolicy();
  Long getMultiAssetId();
}
