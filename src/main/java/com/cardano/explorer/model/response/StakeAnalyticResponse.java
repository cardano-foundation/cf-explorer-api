package com.cardano.explorer.model.response;

import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StakeAnalyticResponse {

  BigDecimal activeStake;
  BigDecimal liveStake;
}
