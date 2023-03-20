package com.cardano.explorer.projection;

import java.math.BigInteger;

public interface AddressTokenProjection {

  BigInteger getQuantity();
  String getFingerprint();
  String getTokenName();
  String getAddress();
  String getPolicy();
}
