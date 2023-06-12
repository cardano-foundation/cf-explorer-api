package org.cardanofoundation.explorer.api.model.request.stake;

import java.util.Date;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.common.validate.length.LengthValid;

@Getter
@Setter
@EqualsAndHashCode
public class StakeLifeCycleFilterRequest {
  @LengthValid(CommonConstant.TX_HASH_LENGTH)
  private String txHash;
  private Date fromDate;
  private Date toDate;

}
