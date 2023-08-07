package org.cardanofoundation.explorer.api.test.projection;

import java.math.BigInteger;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.experimental.FieldDefaults;

import org.cardanofoundation.explorer.api.projection.TokenVolumeProjection;

@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class TokenVolumeProjectionImpl implements TokenVolumeProjection {
  Long ident;
  BigInteger volume;
  @Override
  public Long getIdent() {
    return this.ident;
  }

  @Override
  public BigInteger getVolume() {
    return this.volume;
  }
}
