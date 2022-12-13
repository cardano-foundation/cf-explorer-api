package com.cardano.explorer.common.enumeration;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum AnalyticType {
  ONE_DAY("1d"),
  THREE_DAY("3d"),
  ONE_WEEK("1w"),
  ONE_MONTH("1m");

  String value;
}
