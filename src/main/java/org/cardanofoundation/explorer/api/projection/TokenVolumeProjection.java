package org.cardanofoundation.explorer.api.projection;


import java.math.BigInteger;

public interface TokenVolumeProjection {
  Long getIdent();
  BigInteger getVolume();

}
