package org.cardanofoundation.explorer.api.common.constant;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;

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

  public static final String POOL_STATUS_ACTIVE = "ACTIVE";

  public static final String POOL_STATUS_RETIRING = "RETIRING";

  public static final String MAINNET_ADDRESS_PREFIX = "addr";

  public static final String LOVELACE = "LOVELACE";

  public static final String JWT = "jwt:blacklist:";

  public static final int HASH_LENGTH = 31;


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
   *  check timestamp in range of startTime and  endTime or not
   *  startTime < endTime
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
