package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceOverviewResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingChartResponse;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

public interface GovernanceActionService {
  BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String voterHash, GovernanceActionFilter governanceActionFilter, Pageable pageable);

  GovernanceActionDetailsResponse getGovernanceActionDetails(
      String voterHash, GovernanceActionRequest governanceActionRequest);

  VotingChartResponse getVotingChartByGovActionTxHashAndIndex(
      String txHash, Integer index, VoterType voterType);

  BaseFilterResponse<GovernanceActionResponse> getGovCommitteeStatusHistory(
      GovCommitteeHistoryFilter govCommitteeHistoryFilter, Pageable pageable);

  GovernanceOverviewResponse getGovernanceOverview();
}
