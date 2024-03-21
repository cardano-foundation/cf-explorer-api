package org.cardanofoundation.explorer.api.model.response.drep;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class VotingProcedureChartResponse {
  private String dRepHash;

  private String govActionType;

  private Long numberOfYesVote;
  private Long numberOfNoVotes;
  private Long numberOfAbstainVotes;
}
