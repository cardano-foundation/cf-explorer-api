package com.cardano.explorer.model.response.address;

import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class AddressTokenResponse {
  private String tokenName;
  private BigDecimal quantity;
}
