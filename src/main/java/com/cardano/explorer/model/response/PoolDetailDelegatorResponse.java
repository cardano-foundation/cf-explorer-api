package com.cardano.explorer.model.response;

import com.cardano.explorer.model.response.pool.projection.PoolDetailDelegatorProjection;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailDelegatorResponse implements Serializable {

  private Long id;

  private String address;

  private Long stakeAddressId;

  private BigDecimal totalStake;

  private Timestamp time;

  private BigDecimal fee;

  public PoolDetailDelegatorResponse(PoolDetailDelegatorProjection poolDetailDelegator) {
    this.id = poolDetailDelegator.getId();
    this.address = poolDetailDelegator.getAddress();
    this.stakeAddressId = poolDetailDelegator.getStakeAddressId();
  }
}
