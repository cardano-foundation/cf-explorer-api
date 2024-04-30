package org.cardanofoundation.explorer.api.model.response.governanceAction;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.VoterType;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class VotingChart {
  VoterType voterType;
  private Long numberOfYesVote;
  private Long numberOfNoVotes;
  private Long numberOfAbstainVotes;
}
