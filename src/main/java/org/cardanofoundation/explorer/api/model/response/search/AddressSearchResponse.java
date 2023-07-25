package org.cardanofoundation.explorer.api.model.response.search;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class AddressSearchResponse {
  private String address;
  private boolean isPaymentAddress = false;
  private boolean isStakeAddress = false;
}
