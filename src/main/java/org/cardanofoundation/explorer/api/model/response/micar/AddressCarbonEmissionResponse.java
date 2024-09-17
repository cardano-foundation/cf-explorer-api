package org.cardanofoundation.explorer.api.model.response.micar;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AddressCarbonEmissionResponse {
  String address;
  String stakeAddress;
  Long txCount;
  Double carbonEmissionPerTx;
}
