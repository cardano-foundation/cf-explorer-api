package com.cardano.explorer.common.constant;

import java.math.BigDecimal;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommonConstant {

  public static final int SCALE = 5;

  public static final String PREFIX_POOL_NAME = "{\"name\": \"";

  public static final Integer ZERO = 0;

  public static final BigDecimal TOTAL_ADA = new BigDecimal(45000000000000000L);

  public static final BigDecimal EPOCH_IN_YEARS = new BigDecimal(73);
}
