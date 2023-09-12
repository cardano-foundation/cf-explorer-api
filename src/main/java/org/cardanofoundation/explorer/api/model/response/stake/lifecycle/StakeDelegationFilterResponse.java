package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonInclude;

@Getter
@Setter
@Builder
@JsonInclude
public class StakeDelegationFilterResponse {
  private String txHash;
  private BigInteger outSum;
  private BigInteger fee;
  private LocalDateTime time;
  private String poolName;
  private String poolId;
}
