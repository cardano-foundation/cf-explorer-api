package org.cardanofoundation.explorer.api.model.response;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.*;

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
  private Integer slotNo;
  private Long numberOfStakes;
  private BigInteger rewards;
}
