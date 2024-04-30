package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;

public interface CountVoteOnGovActionProjection {

  String getVoterHash();

  Vote getVote();

  VoterType getVoterType();
}
