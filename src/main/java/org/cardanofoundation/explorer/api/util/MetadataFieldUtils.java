package org.cardanofoundation.explorer.api.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import lombok.extern.log4j.Log4j2;

import com.bloxbean.cardano.client.util.HexUtil;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.FormatFieldType;

@Log4j2
public class MetadataFieldUtils {

  public static final String BASE64_PREFIX = "data:";
  public static final String RAW_BYTE_PREFIX = "0x";
  public static final String BASE64 = "base64";
  public static final String[] MEDIA_TYPE_PREFIX = {"image/", "application/", "audio/", "example/",
      "font/", "model/", "text/", "video/"};
  public static final String[] IMAGE_PREFIX = {"http://", "https://", "ipfs://", "ar://"};
  private static final String SONG_DURATION_REGEX = "^P(?!$)(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+S)?)$";

  public static FormatFieldType getFormatTypeByObject(Object object) {
    if (object instanceof String str) {
      if (Arrays.stream(IMAGE_PREFIX).anyMatch(str::startsWith)
          || str.startsWith(BASE64_PREFIX) && str.contains(BASE64)) {
        return FormatFieldType.URI;
      }
      if (str.startsWith(MEDIA_TYPE_PREFIX[0])) {
        return FormatFieldType.IMAGE_SLASH_MIME_SUB_TYPE;
      }
      if (Arrays.stream(MEDIA_TYPE_PREFIX).anyMatch(str::startsWith)) {
        return FormatFieldType.MIME_TYPE;
      }
      if (str.matches(SONG_DURATION_REGEX)) {
        return FormatFieldType.STRING_ISO8601_DURATION_FORMAT;
      }
      if (isHexString(str)) {
        return FormatFieldType.RAW_BYTES;
      }
      return FormatFieldType.STRING;
    } else if (object instanceof Integer) {
      return FormatFieldType.INTEGER;
    } else if (object instanceof Long) {
      return FormatFieldType.LONG;
    } else if (object instanceof Double) {
      return FormatFieldType.DOUBLE;
    } else if (object instanceof Boolean) {
      return FormatFieldType.BOOLEAN;
    } else if (object instanceof ArrayList<?> arr) {
      if (arr.size() >= 2 &&
          Arrays.stream(CommonConstant.IMAGE_PREFIX).anyMatch(arr.get(0).toString()::startsWith)) {
        return FormatFieldType.URI_ARRAY_PARTS;
      }
      if (arr.stream().allMatch(String.class::isInstance)) {
        return FormatFieldType.ARRAY_STRING;
      }
      return FormatFieldType.ARRAY;
    } else if (object == null) {
      return FormatFieldType.NULL_OR_EMPTY;
    } else if (object instanceof HashMap<?, ?> linkMap) {
      if (linkMap.entrySet().stream()
          .allMatch(entry -> entry.getKey() instanceof String
              && entry.getValue() instanceof String)) {
        return FormatFieldType.MAP_STRING_STRING;
      }
    }

    return FormatFieldType.UNKNOWN;
  }

  public static boolean isHexString(String str) {
    try {
      HexUtil.decodeHexString(str);
      return str.startsWith(RAW_BYTE_PREFIX);
    } catch (Exception ex) {
      log.error("String is not raw bytes");
      log.error("Error: " + ex.getMessage());
    }
    return false;
  }
}
