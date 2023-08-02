package org.cardanofoundation.explorer.api.model.request.stake;

import java.util.Date;
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
  private String txHash;
  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date fromDate;
  @DateValid(pattern = DatePattern.YYYY_MM_DD)
  private Date toDate;
}