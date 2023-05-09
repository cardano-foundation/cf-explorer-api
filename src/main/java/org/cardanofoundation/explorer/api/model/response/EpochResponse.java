package org.cardanofoundation.explorer.api.model.response;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

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
}
