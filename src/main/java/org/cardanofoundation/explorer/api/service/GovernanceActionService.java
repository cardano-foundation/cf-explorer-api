package org.cardanofoundation.explorer.api.service;

import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionFilter;
import org.cardanofoundation.explorer.api.model.request.governanceAction.GovernanceActionRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;

public interface GovernanceActionService {
  BaseFilterResponse<GovernanceActionResponse> getGovernanceActions(
      String dRepHashOrDRepId, GovernanceActionFilter governanceActionFilter, Pageable pageable);

  GovernanceActionDetailsResponse getGovernanceActionDetails(
      String dRepHashOrPoolHash, GovernanceActionRequest governanceActionRequest);
}
