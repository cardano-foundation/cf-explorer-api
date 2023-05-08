package org.cardanofoundation.explorer.api.common.enumeration;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum AnalyticType {
  ONE_DAY("1d"),
  ONE_WEEK("1w"),
  TWO_WEEK("2w"),
  ONE_MONTH("1m"),
  THREE_MONTH("3m");

  String value;
}
