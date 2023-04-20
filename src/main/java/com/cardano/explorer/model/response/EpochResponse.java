package com.cardano.explorer.model.response;

import com.cardano.explorer.common.enumeration.EpochStatus;
import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EpochResponse {

  private Integer no;

  private EpochStatus status;

  private Integer blkCount;

  private BigInteger outSum;

  private Integer txCount;

  private LocalDateTime startTime;

  private LocalDateTime endTime;

  private Integer maxSlot;

  private BigInteger rewardsDistributed;
}
