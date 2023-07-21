package org.cardanofoundation.explorer.api.model.response;

import lombok.*;
import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import java.math.BigInteger;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
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
