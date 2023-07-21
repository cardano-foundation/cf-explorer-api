package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.math.BigInteger;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
public class StakeRegistrationDetailResponse {

  private String txHash;
  private BigInteger fee;
  private Long deposit;
  private LocalDateTime time;
  private boolean joinDepositPaid;
}
