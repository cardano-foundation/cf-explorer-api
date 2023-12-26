package org.cardanofoundation.explorer.api.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.MetadataField;
import org.cardanofoundation.explorer.api.model.metadatastandard.BaseProperty;

import java.util.*;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class MetadataCIP20Utils {
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

      var msg = metadataMap.get(MetadataField.MSG.getName());
      requiredProperties.add(msg(msg, "1"));

    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Check standard CIP-20 fail");
    }

    metadataCIP.put(FIELD_REQUIRED_PROPERTIES, requiredProperties);
    metadataCIP.put(FIELD_VALID, isCIPValid(requiredProperties));
    return metadataCIP;
  }

  public static BaseProperty msg(Object msg, String index){
      return BaseProperty.builder()
                      .index(index)
                      .property(MetadataField.MSG.getName())
                      .format(CommonConstant.FIELD_TYPE[13])
                      .valid(isMsgValid(msg))
                      .value(msg)
                      .build();
  }

  public static boolean isMsgValid(Object msg){
      if (Objects.nonNull(msg) && msg instanceof ArrayList<?>){
          List<?> messages = (ArrayList<?>) msg;
          if (messages.stream().allMatch(m -> m instanceof String
                  && ((String) m).length() <= 64)){
              return true;
          }
      }
      return false;
  }

  public static boolean isCIPValid(List<BaseProperty> baseProperties){
      if (baseProperties.isEmpty())
          return false;
      return baseProperties.stream().allMatch(baseProperty -> baseProperty.getValid().equals(Boolean.TRUE));
  }

}
