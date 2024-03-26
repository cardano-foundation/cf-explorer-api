package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;

public interface CountVoteOnGovActionProjection {

  Long getCount();

  Vote getVote();

  VoterType getVoterType();
}
