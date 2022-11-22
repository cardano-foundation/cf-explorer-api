package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface CollateralInputOutputProjection {
  String getAddress();
  String getTxHash();
  BigDecimal getValue();
}
