package com.cardano.explorer.entity.projection;

import java.math.BigDecimal;

public interface AddressInputOutput {

  Long getTxId();

  String getAddress();

  String getStakeAddress();

  BigDecimal getValue();
}
