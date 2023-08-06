package org.cardanofoundation.explorer.api.test.projection;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.TokenNumberHoldersProjection;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class TokenNumberHoldersProjectionImpl implements TokenNumberHoldersProjection {
  Long ident;
  Long numberOfHolders;

  @Override
  public Long getIdent() {
    return this.ident;
  }

  @Override
  public Long getNumberOfHolders() {
    return this.numberOfHolders;
  }
}
