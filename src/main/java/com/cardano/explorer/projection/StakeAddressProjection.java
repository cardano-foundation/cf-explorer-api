package com.cardano.explorer.projection;

import java.math.BigDecimal;

public interface StakeAddressProjection {

  Long getAddress();

  BigDecimal getTotalStake();
}
