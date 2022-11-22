package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface AddressInputOutputProjection {

  Long getTxId();

  String getTxHash();

  String getAddress();

  String getStakeAddress();

  BigDecimal getValue();
}
