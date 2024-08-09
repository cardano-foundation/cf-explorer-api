package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VotingOnGovActionResponse {

  String voterHash;

  Date timestamp;

  VoterType voterType;

  BigInteger votingStake;

  Double votingPower;

  Vote vote;

  Boolean isRepeatVote;
}
