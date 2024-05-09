package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface DRepInfoProjection {
  String getDrepHash();

  BigInteger getActiveVoteStake();
}
