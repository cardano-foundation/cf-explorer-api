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
public enum GovActionStatus {
  ANY("ANY"),
  OPEN_BALLOT("OPEN_BALLOT"),
  RATIFIED("RATIFIED"),
  ENACTED("ENACTED"),
  EXPIRED("EXPIRED");

  String value;

  private static final Map<String, GovActionStatus> govActionStatusMap = new HashMap<>();

  static {
    for (GovActionStatus type : GovActionStatus.values()) {
      govActionStatusMap.put(type.value, type);
    }
  }

  public static GovActionStatus fromValue(String value) {
    return govActionStatusMap.get(value);
  }
}
