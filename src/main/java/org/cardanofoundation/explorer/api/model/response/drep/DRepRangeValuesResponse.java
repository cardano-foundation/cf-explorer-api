package org.cardanofoundation.explorer.api.model.response.drep;

import java.math.BigInteger;

import lombok.Data;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
public class DRepRangeValuesResponse {
  Double minVotingPower;
  Double maxVotingPower;

  BigInteger maxActiveVoteStake;

  BigInteger minActiveVoteStake;

  Double minGovParticipationRate;

  Double maxGovParticipationRate;
}
