package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

public interface CountVoteOnGovActionProjection {

  String getVoterHash();

  Vote getVote();

  VoterType getVoterType();
}
