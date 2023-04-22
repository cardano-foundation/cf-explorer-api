package com.cardano.explorer.model.request.stake;

import java.time.LocalDateTime;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeLifeCycleFilterRequest {

  private String txHash;
  private LocalDateTime fromDate;
  private LocalDateTime toDate;

}
