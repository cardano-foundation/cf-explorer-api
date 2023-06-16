package org.cardanofoundation.explorer.api.model.response.stake;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.math.BigInteger;
import java.time.LocalDate;

@Getter
@Setter
@AllArgsConstructor
public class StakeAnalyticBalanceResponse {
  private LocalDate date;
  private BigInteger value;
}
