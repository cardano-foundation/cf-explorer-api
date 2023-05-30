package org.cardanofoundation.explorer.api.model.response.stake;

import java.math.BigInteger;
import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class StakeAnalyticBalanceResponse {
  private LocalDate date;
  private BigInteger value;
}
