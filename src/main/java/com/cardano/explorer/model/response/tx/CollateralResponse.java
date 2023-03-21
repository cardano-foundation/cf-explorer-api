package com.cardano.explorer.model.response.tx;

import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class CollateralResponse {
  private String address;
  private String txHash;
  private BigDecimal amount;
}
