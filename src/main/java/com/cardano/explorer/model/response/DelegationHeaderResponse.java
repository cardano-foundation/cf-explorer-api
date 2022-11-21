package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class DelegationHeaderResponse implements Serializable {

  private Integer epochNo;

  private String countDownEndTime;

  private Integer epochSlotNo;

  private BigDecimal liveStake;

  private Integer delegators;
}
