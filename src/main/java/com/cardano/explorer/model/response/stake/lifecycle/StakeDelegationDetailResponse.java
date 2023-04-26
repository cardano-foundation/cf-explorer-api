package com.cardano.explorer.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeDelegationDetailResponse {

  private String txHash;
  private Long blockNo;
  private Integer epoch;
  private BigInteger outSum;
  private BigInteger fee;
  private String poolId;
  private String poolName;
  private LocalDateTime time;
  private BigInteger stakeTotalAmount;

}
