package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeWithdrawalFilterResponse {
  private String txHash;
  private BigInteger value;
  private BigInteger fee;
  private LocalDateTime time;
}
