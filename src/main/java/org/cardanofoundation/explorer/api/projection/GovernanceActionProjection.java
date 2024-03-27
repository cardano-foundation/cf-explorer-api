package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

public interface GovernanceActionProjection {
  String getTxHash();

  Integer getIndex();

  GovActionType getType();

  Integer getSlot();

  Vote getVote();

  Boolean getRepeatVote();

  GovActionStatus getStatus();

  BigInteger getVotingPower();
}
