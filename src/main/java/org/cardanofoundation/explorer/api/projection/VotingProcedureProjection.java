package org.cardanofoundation.explorer.api.projection;

import org.cardanofoundation.explorer.common.entity.enumeration.Vote;

public interface VotingProcedureProjection {
  String getGovActionTxHash();

  Integer getGovActionIndex();

  String getGovActionType();

  String getVoterHash();

  String getVotingProcedureTxHash();

  Integer getVotingProcedureTxIndex();

  Long getBlockTime();

  Vote getVote();
}
