package org.cardanofoundation.explorer.api.model.request.stake;

import java.util.Date;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;

@Getter
@Setter
@EqualsAndHashCode
public class StakeLifeCycleFilterRequest {

  private String txHash;
  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date fromDate;
  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date toDate;
}
