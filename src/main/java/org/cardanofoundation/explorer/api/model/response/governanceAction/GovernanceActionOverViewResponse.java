package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.util.Date;

import lombok.*;

import com.fasterxml.jackson.annotation.JsonProperty;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GovernanceActionOverViewResponse {

  String txHash;

  Integer index;

  Date dateCreated;

  GovActionType actionType;

  GovActionStatus status;

  @JsonProperty("abstract")
  String abstractContent;

  String motivation;

  String rationale;

  Boolean isValidHash;

  String anchorHash;

  String anchorUrl;

  Boolean allowedVoteBySPO;

  Boolean allowedVoteByCC;
}
