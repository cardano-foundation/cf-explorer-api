package org.cardanofoundation.explorer.api.model.response.stake;

import java.math.BigInteger;
import java.time.LocalDate;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeAnalyticBalanceResponse {
  private LocalDate date;
  private BigInteger value;
}
