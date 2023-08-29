package org.cardanofoundation.explorer.api.common.enumeration;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import org.apache.commons.lang3.StringUtils;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum DestinationPrefix {
  TOKEN("/user/queue/token"),
  BLOCK("/user/queue/block");

  String value;

  public static DestinationPrefix from(String value) {
    if (StringUtils.isNotEmpty(value)) {
      for (DestinationPrefix destinationPrefix : DestinationPrefix.values()) {
        if (destinationPrefix.getValue().equals(value)) {
          return destinationPrefix;
        }
      }
    }
    throw new IllegalArgumentException("Invalid destination prefix: " + value);
  }
}
