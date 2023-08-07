package org.cardanofoundation.explorer.api.test.projection;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class StakeKeyProjectionImpl implements StakeKeyProjection {
  Long poolUpdateId;
  String view;

  @Override
  public Long getPoolUpdateId() {
    return this.poolUpdateId;
  }

  @Override
  public String getView() {
    return this.view;
  }
}
