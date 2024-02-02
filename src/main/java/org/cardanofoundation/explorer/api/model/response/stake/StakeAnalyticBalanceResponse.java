package org.cardanofoundation.explorer.api.model.response.stake;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class StakeAnalyticBalanceResponse {
  private LocalDateTime date;
  private BigInteger value;
}
