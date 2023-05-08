package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolInfoResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private BigInteger poolSize;

  private BigInteger rewardAvailable;

  private String status;

  private Integer epochNo;

  List<String> stakeKeys;
}
