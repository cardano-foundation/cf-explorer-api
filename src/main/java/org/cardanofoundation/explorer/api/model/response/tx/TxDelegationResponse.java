package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class TxDelegationResponse {
  private String address;
  private String poolId;
}
