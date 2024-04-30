package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class GovernanceActionDetailsResponse extends GovernanceActionDetails {
  List<HistoryVote> historyVotes;
}
