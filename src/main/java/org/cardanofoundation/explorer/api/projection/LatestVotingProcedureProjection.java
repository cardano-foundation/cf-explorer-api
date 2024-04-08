package org.cardanofoundation.explorer.api.projection;

import java.math.BigInteger;

import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

public interface LatestVotingProcedureProjection {
  String getTxHash();

  Integer getIdx();

  VoterType getVoterType();

  String getVoterHash();

  String getGovActionTxHash();

  Integer getGovActionIndex();

  Vote getVote();

  String getAnchorUrl();

  String getAnchorHash();

  Integer getEpoch();

  Integer getBlock();

  BigInteger getBlockTime();

  Boolean getRepeatVote();
}
