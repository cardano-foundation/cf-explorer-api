package com.cardano.explorer.model.request.stake;

import java.util.Date;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeLifeCycleFilterRequest {

  private String txHash;
  private Date fromDate;
  private Date toDate;

}
