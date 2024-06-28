package org.cardanofoundation.explorer.api.model.response.committee;

import java.util.Date;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.FieldDefaults;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.enumeration.CommitteeState;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class CommitteeOverviewResponse {

  CommitteeState currentState;
  String proposalPolicy;
  Long activeMembers;
  Double threshold;
  Long governanceVotes;
  Date lastUpdate;
}
