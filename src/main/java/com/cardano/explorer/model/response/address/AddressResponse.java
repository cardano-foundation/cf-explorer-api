package com.cardano.explorer.model.response.address;

import java.math.BigDecimal;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AddressResponse {
  private String address;
  private Integer txCount;
  private BigDecimal balance;
  private List<AddressTokenResponse> tokens;
  private String stakeAddress;

}
