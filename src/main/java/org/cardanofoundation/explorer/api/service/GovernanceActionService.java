package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.governanceAction.GovCommitteeHistoryFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.request.governanceAction.VoteFilter;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.*;
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

  GovernanceActionOverViewResponse getGovernanceActionInfo(String txHash, Integer index);

  BaseFilterResponse<AuthorResponse> getAuthorsByAnchor(
      String anchorUrl, String anchorHash, Pageable pageable);

  BaseFilterResponse<VotingOnGovActionResponse> getVotingOnGovAction(
      VoteFilter voteFilter, Pageable pageable);

  RangeFilterVoteResponse getRangeFilterVoteResponse(String txHash, Integer index);
}
