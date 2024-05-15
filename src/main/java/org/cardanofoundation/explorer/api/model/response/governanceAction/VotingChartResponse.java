package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class VotingChartResponse {
  String txHash;
  Integer index;
  VoterType voterType;

  BigInteger activeVoteStake;
  BigInteger abstainVoteStake;
  BigInteger totalYesVoteStake;
  BigInteger totalNoVoteStake;

  Double threshold;

  Long ccMembers;
  Long abstainCcMembers;
  Long yesCcMembers;
  Long noCcMembers;
}
