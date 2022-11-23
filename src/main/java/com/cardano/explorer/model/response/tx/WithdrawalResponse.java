package com.cardano.explorer.model.response.tx;

import java.math.BigDecimal;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class WithdrawalResponse {
  private String stakeAddressFrom;
  private List<String> addressTo;
  private BigDecimal amount;
}
