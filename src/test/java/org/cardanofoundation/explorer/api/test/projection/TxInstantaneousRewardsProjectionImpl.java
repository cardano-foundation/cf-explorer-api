package org.cardanofoundation.explorer.api.test.projection;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.TxInstantaneousRewardsProjection;

@Builder
@FieldDefaults(level = AccessLevel.PRIVATE)
public class TxInstantaneousRewardsProjectionImpl implements TxInstantaneousRewardsProjection {
  String stakeAddress;
  String amount;

  @Override
  public String getStakeAddress() {
    return this.stakeAddress;
  }

  @Override
  public String getAmount() {
    return this.amount;
  }
}
