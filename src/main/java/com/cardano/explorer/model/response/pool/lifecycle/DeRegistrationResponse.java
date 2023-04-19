package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class DeRegistrationResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private BigInteger poolHold;

  private String txHash;

  private BigInteger totalFees;

  private Timestamp time;

  private BigInteger fees;

  private List<String> stakeKeys;

  private Integer retiringEpoch;
}
