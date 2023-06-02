package org.cardanofoundation.explorer.api.test.projection;

import lombok.Builder;

import org.cardanofoundation.explorer.api.projection.UniqueAddressProjection;

@Builder
public class UniqueAccountProjectionImpl implements UniqueAddressProjection {

  private Long id;
  private String address;

  @Override
  public Long getId() {
    return id;
  }

  @Override
  public String getAddress() {
    return address;
  }
}
