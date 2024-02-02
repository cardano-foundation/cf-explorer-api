package org.cardanofoundation.explorer.api.model.response;

import java.math.BigInteger;
import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;

@Getter
@Setter
@Builder
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

  private Integer account;
}
