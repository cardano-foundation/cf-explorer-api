package com.cardano.explorer.model.response.tx;

import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class CollateralOutputResponse {
  private String address;
  private BigDecimal amount;
}
