package com.cardano.explorer.model.response.stake;

import java.io.Serializable;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeFilterResponse implements Serializable {

  private String stakeKey;

  private BigDecimal balance;

  private String poolId;

  private String tickerName;

  private String poolName;

}
