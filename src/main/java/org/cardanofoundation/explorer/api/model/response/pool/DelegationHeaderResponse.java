package org.cardanofoundation.explorer.api.model.response.pool;

import java.io.Serializable;
import java.math.BigInteger;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DelegationHeaderResponse implements Serializable {

  private Integer epochNo;

  private Long countDownEndTime;

  private Long epochSlotNo;

  private BigInteger liveStake;

  private Integer delegators;

  private Integer activePools;

  private Integer retiredPools;
}
