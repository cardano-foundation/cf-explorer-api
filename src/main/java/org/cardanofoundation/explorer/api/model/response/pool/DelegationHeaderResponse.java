package org.cardanofoundation.explorer.api.model.response.pool;

import java.io.Serializable;
import java.math.BigInteger;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class DelegationHeaderResponse implements Serializable {

  private Integer epochNo;

  private Long countDownEndTime;

  private Integer epochSlotNo;

  private BigInteger liveStake;

  private Integer delegators;

  private Integer activePools;

  private Integer retiredPools;
}
