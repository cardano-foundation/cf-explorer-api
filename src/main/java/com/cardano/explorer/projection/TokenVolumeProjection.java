package com.cardano.explorer.projection;


import java.math.BigInteger;

public interface TokenVolumeProjection {
  Long getIdent();
  BigInteger getVolume();

}
