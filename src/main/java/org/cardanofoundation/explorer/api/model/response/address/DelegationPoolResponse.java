package org.cardanofoundation.explorer.api.model.response.address;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class DelegationPoolResponse {
  private String tickerName;
  private String poolName;
  private String poolId;

}
