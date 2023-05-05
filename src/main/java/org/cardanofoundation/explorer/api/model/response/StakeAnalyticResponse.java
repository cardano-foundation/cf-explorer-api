package org.cardanofoundation.explorer.api.model.response;

import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeAnalyticResponse {

  BigInteger activeStake;
  BigInteger liveStake;
}
