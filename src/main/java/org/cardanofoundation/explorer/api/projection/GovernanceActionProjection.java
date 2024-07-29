package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;

public interface GovernanceActionProjection {
  String getTxHash();

  Integer getIndex();

  GovActionType getType();

  BigInteger getIndexType();

  Integer getSlot();

  Vote getVote();

  Boolean getRepeatVote();

  GovActionStatus getStatus();

  BigInteger getVotingPower();

  String getVoterHash();

  Long getCreatedAt();

  Long getGovCount();
}
