package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.JsonNode;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class GovernanceActionDetails {
  String txHash;

  Long index;

  GovActionType govActionType;

  String anchorHash;

  String anchorUrl;

  JsonNode details;

  @JsonIgnore Integer epoch;

  @JsonIgnore Long blockTime;

  Vote voteType;

  String voterHash;

  GovActionStatus status;

  BigInteger votingPower;

  Date submissionDate;

  Date expiryDate;

  String poolName;

  Boolean allowedVoteBySPO;

  Boolean allowedVoteByCC;
}
