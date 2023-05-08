package org.cardanofoundation.explorer.api.model.response.stake;

import java.io.Serializable;
import java.math.BigInteger;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeFilterResponse implements Serializable {

  private String stakeKey;

  private BigInteger balance;

  private String poolId;

  private String tickerName;

  private String poolName;

}
