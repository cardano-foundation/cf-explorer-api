package com.cardano.explorer.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeDelegationFilterResponse {
  private String txHash;
  private BigInteger outSum;
  private BigInteger fee;
  private LocalDateTime time;

}
