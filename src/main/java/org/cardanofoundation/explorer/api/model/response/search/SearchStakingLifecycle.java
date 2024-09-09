package org.cardanofoundation.explorer.api.model.response.search;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SearchStakingLifecycle {
  private AddressSearchResponse address;
  private PoolSearchResponse pool;
  private boolean validPoolName;
}
