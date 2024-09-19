package org.cardanofoundation.explorer.api.model.response.search;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;

@Getter
@Setter
public class SearchStakingLifecycle {
  private AddressSearchResponse address;
  private BaseFilterResponse<PoolSearchResponse> poolList;
  private boolean validPoolName;
}
