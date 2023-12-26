package org.cardanofoundation.explorer.api.common.enumeration;

import java.util.HashMap;
import java.util.Map;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum FormatFieldType {
  STRING("String"),
  STRING_OR_RAW_BYTES("String | Raw Bytes"),
  URI_OR_ARRAY("URI | Array"),
  IMAGE_SLASH_MIME_SUB_TYPE("image/<mime_sub_type>"),
  STRING_OR_ARRAY_STRING("String | Array<String>"),
  MIME_TYPE("mime_type"),
  VERSION_1_OR_2("1 or 2"),
  URI("URI"),
  ARRAY("Array"),
  RAW_BYTES("Raw Bytes"),
  SINGLE_AND_MULTIPLE("Single / Multiple"),
  INTEGER("Integer"),
  STRING_ISO8601_DURATION_FORMAT("String: ISO8601 Duration Format"),
  ARRAY_STRING("Array<String>"),
  BOOLEAN("Boolean"),
  MAP_STRING_STRING("Map: <String, String>"),
  ARRAY_ARTIST("Array<Artist>"),
  URL("URL"),
  ARTIST("Artist");

  private static final Map<String, FormatFieldType> formatFieldTypeMap = new HashMap<>();

  static {
    for (FormatFieldType type : FormatFieldType.values()) {
      formatFieldTypeMap.put(type.value, type);
    }
  }

  String value;

  public static FormatFieldType fromValue(String value) {
    return formatFieldTypeMap.get(value);
  }
}
