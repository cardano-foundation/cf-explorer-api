package org.cardanofoundation.explorer.api.common.enumeration;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum StakeRewardType {
  REWARD_RECEIVED,
  REWARD_WITHDRAWN
}
