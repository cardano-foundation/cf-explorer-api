package org.cardanofoundation.explorer.api.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.cardanofoundation.explorer.api.common.enumeration.FormatFieldType;
import org.cardanofoundation.explorer.api.common.enumeration.MetadataField;
import org.cardanofoundation.explorer.api.model.metadatastandard.BaseProperty;

import java.util.*;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class MetadataCIP83Utils {

  private static final String FIELD_REQUIRED_PROPERTIES = "requiredProperties";
  private static final String FIELD_VALID = "valid";

  @SuppressWarnings("unchecked")
  public static Map<String, Object> standard(String jsonMetadata) {
    Map<String, Object> metadataCIP = new HashMap<>();
    List<BaseProperty> requiredProperties = new ArrayList<>();

    try {
      ObjectMapper objectMapper = new ObjectMapper();
      Map<Object, Object> metadataMap = objectMapper.readValue(jsonMetadata, new TypeReference<>() {
      });

      var enc = metadataMap.get(MetadataField.ENC.getName());
      var msg = metadataMap.get(MetadataField.MSG.getName());

      requiredProperties.add(MetadataCIP20Utils.msg(msg, "1"));
      requiredProperties.add(enc(enc, "2"));


    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Check standard CIP-83 fail");
    }

    metadataCIP.put(FIELD_REQUIRED_PROPERTIES, requiredProperties);
    metadataCIP.put(FIELD_VALID, isCIPValid(requiredProperties));
    return metadataCIP;
  }

  public static BaseProperty enc(Object enc, String index) {
    FormatFieldType formatFieldType = MetadataFieldUtils.getFormatTypeByObject(enc);
    boolean isValid = FormatFieldType.STRING.equals(formatFieldType)
        || FormatFieldType.STRING.equals(formatFieldType.getParentType());
    return BaseProperty.builder()
        .index(index)
        .property(MetadataField.ENC.getName())
        .format(FormatFieldType.STRING.getValue())
        .valid(isValid)
        .valueFormat(isValid ? FormatFieldType.STRING.getValue()
                             : formatFieldType.getValue())
        .value(enc)
        .build();
  }

  public static boolean isCIPValid(List<BaseProperty> baseProperties) {
    if (baseProperties.isEmpty()) {
      return false;
    }
    return baseProperties.stream()
        .allMatch(baseProperty -> baseProperty.getValid().equals(Boolean.TRUE));
  }
}
