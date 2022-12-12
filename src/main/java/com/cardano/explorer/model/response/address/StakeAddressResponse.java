package com.cardano.explorer.model.response.address;

import com.cardano.explorer.common.enumeration.StakeAddressStatus;
import java.math.BigDecimal;
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
  private BigDecimal totalStake;
  private BigDecimal rewardAvailable;
  private BigDecimal rewardWithdrawn;
  private DelegationPoolResponse pool;
}
