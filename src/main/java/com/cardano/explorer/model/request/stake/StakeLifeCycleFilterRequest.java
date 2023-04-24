package com.cardano.explorer.model.request.stake;

import java.util.Date;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@EqualsAndHashCode
public class StakeLifeCycleFilterRequest {

  private String txHash;
  private Date fromDate;
  private Date toDate;

}
