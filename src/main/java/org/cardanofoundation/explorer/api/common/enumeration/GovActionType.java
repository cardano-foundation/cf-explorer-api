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
public enum GovActionType {
  PARAMETER_CHANGE_ACTION("PARAMETER_CHANGE_ACTION"),
  HARD_FORK_INITIATION_ACTION("HARD_FORK_INITIATION_ACTION"),
  TREASURY_WITHDRAWALS_ACTION("TREASURY_WITHDRAWALS_ACTION"),
  NO_CONFIDENCE("NO_CONFIDENCE"),
  UPDATE_COMMITTEE("UPDATE_COMMITTEE"),
  NEW_CONSTITUTION("UPDATE_CONSTITUTION"),
  INFO_ACTION("INFO_ACTION"),
  ALL("ALL");
  String value;

  private static final Map<String, GovActionType> govActionTypeMap = new HashMap<>();

  static {
    for (GovActionType type : GovActionType.values()) {
      govActionTypeMap.put(type.value, type);
    }
  }

  public static GovActionType fromValue(String value) {
    return govActionTypeMap.get(value);
  }
}
