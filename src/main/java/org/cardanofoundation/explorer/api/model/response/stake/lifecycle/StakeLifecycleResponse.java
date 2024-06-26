package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeLifecycleResponse {
  private Boolean hasRegistration;
  private Boolean hasDelegation;
  private Boolean hashRewards;
  private Boolean hasWithdrawal;
  private Boolean hasDeRegistration;
  private BigInteger totalOperatorRewards;
  private BigInteger totalDelegatorRewards;
}
