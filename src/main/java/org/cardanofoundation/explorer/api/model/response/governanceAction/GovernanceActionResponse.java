package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class GovernanceActionResponse {
  String txHash;
  Long index;
  GovActionType type;
  Vote vote;
  GovActionStatus status;
  BigInteger votingPower;
  BigInteger indexType;
  Boolean isRepeatVote;
  String voterHash;
  Date createdAt;
  String govActionName;
}
