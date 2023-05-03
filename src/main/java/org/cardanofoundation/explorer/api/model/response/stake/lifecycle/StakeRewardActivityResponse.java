package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import org.cardanofoundation.explorer.api.common.enumeration.StakeRewardType;
import java.io.Serializable;
import java.math.BigInteger;
import java.util.Date;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakeRewardActivityResponse implements Serializable {

  private Integer epochNo;
  private BigInteger amount;
  private Date time;
  private StakeRewardType type;
}
