package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface AddressTokenProjection {

  BigDecimal getQuantity();
  String getFingerprint();
  String getTokenName();
  String getAddress();
}
