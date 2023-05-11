package org.cardanofoundation.explorer.api.common.constant;

import java.math.BigDecimal;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommonConstant {

  public static final int SCALE = 6;

  public static final Integer ZERO = 0;

  public static final BigDecimal TOTAL_ADA = new BigDecimal(45000000000000000L);

  public static final BigDecimal EPOCH_IN_YEARS = new BigDecimal(73);

  public static final String MAINNET_NETWORK = "mainnet";

  public static final String TESTNET_ADDRESS_PREFIX = "addr_test";

  public static final String MAINNET_ADDRESS_PREFIX = "addr";

  public static final String REDIS_POOL_PREFIX = "LIVE_STAKE_POOL_";

  public static final String LOVELACE = "LOVELACE";

  public static final BigDecimal PERCENT = new BigDecimal(100);

  public static final String REDIS_TOTAL_LIVE_STAKE = "TOTAL_LIVE_STAKE_";

  public static final String POOL_STATUS_ACTIVE = "ACTIVE";

  public static final String POOL_STATUS_RETIRING = "RETIRING";

  public static final Integer COLUMN_WITH = 255;

  public static final String JWT = "jwt:blacklist:";
}
