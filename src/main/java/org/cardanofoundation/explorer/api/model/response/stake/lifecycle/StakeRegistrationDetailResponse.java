package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

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
