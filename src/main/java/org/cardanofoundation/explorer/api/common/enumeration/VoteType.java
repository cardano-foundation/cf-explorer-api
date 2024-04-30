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
public enum VoteType {
  NO("NO"),
  YES("YES"),
  ABSTAIN("ABSTAIN"),
  ANY("ANY"),
  NONE("NONE");

  String value;

  private static final Map<String, VoteType> voteTypeMap = new HashMap<>();

  static {
    for (VoteType type : VoteType.values()) {
      voteTypeMap.put(type.value, type);
    }
  }

  public static VoteType fromValue(String value) {
    return voteTypeMap.get(value);
  }
}
