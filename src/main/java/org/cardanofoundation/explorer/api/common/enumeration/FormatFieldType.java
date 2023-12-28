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
  STRING(null, "String"),
  ARRAY(null,"Array"),
  STRING_OR_RAW_BYTES(STRING,"String | Raw Bytes"),
  URI_OR_ARRAY(STRING,"URI | Array"),
  URI_ARRAY_PARTS(ARRAY,"URI Array Parts"),
  IMAGE_SLASH_MIME_SUB_TYPE(STRING,"image/<mime_sub_type>"),
  STRING_OR_ARRAY_STRING(null,"String | Array<String>"),
  MIME_TYPE(STRING,"mime_type"),
  VERSION_1_OR_2(null,"1 or 2"),
  URI(STRING,"URI"),
  RAW_BYTES(STRING,"Raw Bytes"),
  SINGLE_AND_MULTIPLE(null,"Single / Multiple"),
  INTEGER(null,"Integer"),
  STRING_ISO8601_DURATION_FORMAT(STRING,"String: ISO8601 Duration Format"),
  ARRAY_STRING(ARRAY,"Array<String>"),
  BOOLEAN(null,"Boolean"),
  MAP_STRING_STRING(null,"Map: <String, String>"),
  ARRAY_ARTIST(ARRAY,"Array<Artist>"),
  URL(STRING,"URL"),
  ARTIST(null,"Artist"),
  NEITHER_VERSION_1_OR_2(null,"Neither version 1 or 2"),
  LONG(null,"Long"),
  DOUBLE(null,"Double"),
  UNKNOWN(null,"Unknown"),
  NULL_OR_EMPTY(null,"")
  ;

  private static final Map<String, FormatFieldType> formatFieldTypeMap = new HashMap<>();

  static {
    for (FormatFieldType type : FormatFieldType.values()) {
      formatFieldTypeMap.put(type.value, type);
    }
  }

  FormatFieldType parentType;
  String value;

  public static FormatFieldType fromValue(String value) {
    return formatFieldTypeMap.get(value);
  }
}
