package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.enumeration.Vote;

public interface VotingCCProjection {

  String getTxHash();

  Integer getIndex();

  Vote getVote();

  String getVoterHash();
}
