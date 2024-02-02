package org.cardanofoundation.explorer.api.model.response.address;

import java.math.BigInteger;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.cardanofoundation.explorer.api.common.enumeration.StakeAddressStatus;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakeAddressResponse {
  private StakeAddressStatus status;
  private String stakeAddress;
  private BigInteger totalStake;
  private BigInteger rewardAvailable;
  private BigInteger rewardWithdrawn;
  private DelegationPoolResponse pool;
  private List<String> rewardPools;
}
