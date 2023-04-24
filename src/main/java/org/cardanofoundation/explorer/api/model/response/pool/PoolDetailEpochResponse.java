package org.cardanofoundation.explorer.api.model.response.pool;

import org.cardanofoundation.explorer.api.json.serialize.PercentSerializer;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDetailEpochProjection;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolDetailEpochResponse implements Serializable {

  private Integer epoch;

  private Long block;

  private BigInteger stakeAmount;

  private BigInteger delegators;

  private BigInteger fee;

  @JsonSerialize(using = PercentSerializer.class)
  private Double ros;

  public PoolDetailEpochResponse(PoolDetailEpochProjection poolEpoch) {
    this.epoch = poolEpoch.getEpochNo();
    this.block = poolEpoch.getCountBlock();
  }
}
