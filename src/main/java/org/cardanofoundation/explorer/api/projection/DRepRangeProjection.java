package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

public interface DRepRangeProjection {
  BigInteger getMinActiveVoteStake();

  BigInteger getMaxActiveVoteStake();

  Double getMinVotingPower();

  Double getMaxVotingPower();
}
