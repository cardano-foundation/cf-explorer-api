package org.cardanofoundation.explorer.api.model.request.governanceAction;

import java.time.LocalDateTime;

import jakarta.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.springframework.format.annotation.DateTimeFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;
import org.cardanofoundation.explorer.common.entity.enumeration.Vote;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class GovernanceActionFilter {

  Boolean isRepeatVote;

  String governanceActionTxHash;

  String anchorText;

  @NotNull GovActionType actionType;

  @NotNull GovActionStatus actionStatus;

  @NotNull Vote voteType;

  @NotNull VoterType voterType;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime fromDate;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime toDate;
}
