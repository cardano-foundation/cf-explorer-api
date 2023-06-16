package org.cardanofoundation.explorer.api.model.response;

import lombok.*;

import java.math.BigInteger;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InstantaneousRewardsResponse {
  private String txHash;
  private LocalDateTime time;
  private Long blockNo;
  private Integer epochNo;
  private Integer epochSlotNo;
  private Long numberOfStakes;
  private BigInteger rewards;
}
