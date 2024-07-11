package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionStatus;
import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GovernanceOverviewResponse {
  private Long activeDReps;
  private Long activeSPOs;
  private Long activeCommittees;
  private Long totalGovActions;

  private Map<GovActionType, Long> govCountMap;
  private Map<GovActionStatus, Long> govStatusMap;
}
