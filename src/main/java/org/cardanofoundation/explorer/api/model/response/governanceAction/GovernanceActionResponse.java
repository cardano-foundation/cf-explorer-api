package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

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

  Boolean isRepeatVote;
}
