package org.cardanofoundation.explorer.api.common.constant;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
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

  public static final String POOL_STATUS_ACTIVE = "ACTIVE";

  public static final String POOL_STATUS_RETIRED = "RETIRED";

  public static final String ADDRESS_PREFIX = "addr";

  public static final String STAKE_ADDRESS_PREFIX = "stake";

  public static final String LOVELACE = "LOVELACE";

  public static final BigDecimal PERCENT = new BigDecimal(100);

  public static final String REDIS_TOTAL_LIVE_STAKE = "TOTAL_LIVE_STAKE_";

  public static final int TX_HASH_LENGTH = 64;

  public static final int BLOCK_HASH_LENGTH = 64;

  public static final int STAKE_KEY_LENGTH_MAINNET = 59;

  public static final int STAKE_KEY_LENGTH_TESTNET = 64;

  public static final int POOL_VIEW_LENGTH = 56;

  public static final int TOKEN_FINGERPRINT_LENGTH = 44;

  public static final String JWT = "jwt:blacklist:";

  public static final int HASH_LENGTH = 31;

  public static final String ACTIVATE_STAKE = "ACTIVATE_STAKE_";

  public static final String LIVE_STAKE = "LIVE_STAKE_";

  public static final String REDIS_TOTAL_ACTIVATE_STAKE = "TOTAL_ACTIVATE_STAKE_";

  public static final String REDIS_POOL_ACTIVATE = "POOL_ACTIVATE_";

  public static final String REDIS_POOL_INACTIVATE = "POOL_INACTIVATE_";

  public static final String REDIS_TOTAL_DELEGATOR = "TOTAL_DELEGATOR_";

  public static final String PREFIXED_STAKE_KEY = "stake";

  public static final String PREFIXED_POOL_VIEW = "pool";

  public static final String PREFIXED_TOKEN_FINGERPRINT = "asset";

  public static final String PREPROD_NETWORK = "preprod";

  public static final String PREVIEW_NETWORK = "preview";

  public static final String POOL_IDS_INACTIVATE = "POOL_IDS_INACTIVATE_";

  public static final String UNDERSCORE = "_";

  public static final String DATA_IS_NOT_SYNCING = "Data is not syncing";

  public static final String READY_TO_SERVE = "Ready to serve";

  public static final String SYNCING_BUT_NOT_READY = "Data is syncing, but it isn't ready to serve yet";

  public static final String REPORT_LIMIT_PER_24HOURS = "reportLimitPer24Hours";

  public static final int UNLIMITED_REPORT = -1;

  public static final BigInteger METADATA_LABEL_721 = BigInteger.valueOf(721);

  public static int hashCode(Object... a) {
    if (a == null) {
      return -BigInteger.ONE.intValue();
    }

    int result = BigInteger.ONE.intValue();

    for (Object element : a) {
      result = HASH_LENGTH * result + (element == null ? -BigInteger.ONE.intValue()
          : element.hashCode());
    }

    return result;
  }

  /**
   * check timestamp in range of startTime and  endTime or not startTime < endTime
   *
   * @param timestamp
   * @param startFilterTime
   * @param endFilterTime
   * @return
   */
  public static boolean isWithinRange(Timestamp timestamp,
      Timestamp startFilterTime, Timestamp endFilterTime) {
    return !(timestamp.before(startFilterTime) || timestamp.after(endFilterTime));
  }
}
