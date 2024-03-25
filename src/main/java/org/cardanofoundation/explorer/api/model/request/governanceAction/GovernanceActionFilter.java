package org.cardanofoundation.explorer.api.model.request.governanceAction;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.api.common.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.api.common.enumeration.GovActionType;
import org.cardanofoundation.explorer.api.common.enumeration.VoteType;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class GovernanceActionFilter {

  Boolean isRepeatVote;

  String governanceActionTxHash;

  String anchorText;

  GovActionType actionType;

  GovActionStatus actionStatus;

  VoteType voteType;

  VoterType voterType;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date fromDate;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date toDate;
}