package com.cardano.explorer.model.response.address;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import java.math.BigInteger;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
