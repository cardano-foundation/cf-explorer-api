package org.cardanofoundation.explorer.api.common.enumeration;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum StakeTxType {
  SENT,
  RECEIVED,
  FEE_PAID,
  CERTIFICATE_FEE_PAID,
  CERTIFICATE_HOLD_PAID,
  CERTIFICATE_HOLD_DEPOSIT_REFUNDED,
  REWARD_WITHDRAWN,
  REWARD_WITHDRAWN_AND_CERTIFICATE_HOLD_PAID,
  REWARD_WITHDRAWN_AND_CERTIFICATE_HOLD_DEPOSIT_REFUNDED,
  UNKNOWN;
}
