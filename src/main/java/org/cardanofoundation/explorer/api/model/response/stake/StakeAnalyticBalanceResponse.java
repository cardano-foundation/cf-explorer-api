package org.cardanofoundation.explorer.api.model.response.stake;

import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.math.BigInteger;

@Getter
@Setter
@AllArgsConstructor
public class StakeAnalyticBalanceResponse {
  private LocalDateTime date;
  private BigInteger value;
}
