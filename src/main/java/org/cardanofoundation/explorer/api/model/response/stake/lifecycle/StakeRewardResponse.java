package org.cardanofoundation.explorer.api.model.response.stake.lifecycle;

import java.math.BigInteger;
import java.util.Date;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;

@Getter
@Setter
@AllArgsConstructor
@Builder
public class StakeRewardResponse {
  private Integer epoch;
  private Date time;
  private BigInteger amount;
  private RewardType type;
}
