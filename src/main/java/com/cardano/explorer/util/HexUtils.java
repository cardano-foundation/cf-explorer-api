package com.cardano.explorer.util;

import java.nio.charset.StandardCharsets;
import org.apache.commons.codec.binary.Hex;

public class HexUtils {

  /**
   * Convert from hex to UTF-8
   *
   * @param hexString string hex
   * @return string UTF-8
   */
  public static String fromHex(String hexString) {
    try {
      byte[] bytes = Hex.decodeHex(hexString.toCharArray());
      return new String(bytes, StandardCharsets.UTF_8);
    } catch (Exception ex) {
      return hexString;
    }
  }
}
