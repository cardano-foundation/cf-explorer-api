package org.cardanofoundation.explorer.api.model.request.stake;

import java.util.Date;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.*;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.common.validation.date.DatePattern;
import org.cardanofoundation.explorer.common.validation.date.param.DateValid;
import org.cardanofoundation.explorer.common.validation.length.LengthValid;

@Getter
@Setter
@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor
public class StakeLifeCycleFilterRequest {

  @LengthValid(CommonConstant.TX_HASH_LENGTH)
  @Parameter(description = "The hash identifier of the transaction.")
  private String txHash;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  @Parameter(description = "The date from which the stake life cycle is considered.")
  private Date fromDate;

  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  @Parameter(description = "The date to which the stake life cycle is considered.")
  private Date toDate;

}
