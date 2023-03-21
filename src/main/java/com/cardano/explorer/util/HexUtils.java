package com.cardano.explorer.util;

import com.sotatek.cardano.ledgersync.util.StringUtil;
import java.nio.charset.StandardCharsets;
import org.apache.commons.codec.binary.Hex;

public class HexUtils {

  private HexUtils() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Convert from hex to UTF-8
   *
   * @param hexString string hex
   * @return string UTF-8
   */
  public static String fromHex(String hexString) {
    try {
      byte[] bytes = Hex.decodeHex(hexString.toCharArray());
      if(StringUtil.isUtf8(bytes)){
        return new String(bytes, StandardCharsets.UTF_8);
      }
      return null;
    } catch (Exception ex) {
      return null;
    }
  }
}
