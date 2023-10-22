package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.math.BigInteger;

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
