package org.cardanofoundation.explorer.api.model.response;

import lombok.*;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailDelegatorProjection;
import java.io.Serializable;
import java.math.BigInteger;
import java.sql.Timestamp;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PoolDetailDelegatorResponse implements Serializable {

  private Long stakeAddressId;

  private BigInteger totalStake;

  private Timestamp time;

  private BigInteger fee;

  private String view;

  public PoolDetailDelegatorResponse(PoolDetailDelegatorProjection poolDetailDelegator) {
    this.stakeAddressId = poolDetailDelegator.getStakeAddressId();
    this.fee = poolDetailDelegator.getFee();
    this.time = poolDetailDelegator.getTime();
    this.view = poolDetailDelegator.getView();
  }
}
