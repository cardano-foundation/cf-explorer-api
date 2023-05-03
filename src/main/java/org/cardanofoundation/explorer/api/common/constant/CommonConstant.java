package org.cardanofoundation.explorer.api.common.constant;

import java.math.BigDecimal;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommonConstant {

  public static final int SCALE = 6;

  public static final int SCALE_10 = 10;

  public static final String PREFIX_POOL_NAME = "{\"name\": \"";

  public static final Integer ZERO = 0;

  public static final BigDecimal TOTAL_ADA = new BigDecimal(45000000000000000L);

  public static final BigDecimal EPOCH_IN_YEARS = new BigDecimal(73);

  public static final String MAINNET_NETWORK = "mainnet";

  public static final String TESTNET_ADDRESS_PREFIX = "addr_test";

  public static final String MAINNET_ADDRESS_PREFIX = "addr";

  public static final String LOVELACE = "LOVELACE";
}
