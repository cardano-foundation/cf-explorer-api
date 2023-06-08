package org.cardanofoundation.explorer.api.model.response.pool;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.api.json.serialize.PercentSerializer;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochStakeProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolHistoryKoiOsProjection;

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

  public PoolDetailEpochResponse(EpochStakeProjection projection) {
    this.epoch = projection.getEpochNo();
    this.stakeAmount = projection.getTotalStake();
  }

  public PoolDetailEpochResponse(PoolHistoryKoiOsProjection projection) {
    this.epoch = projection.getEpochNo();
    this.delegators = projection.getDelegateReward();
    this.fee = projection.getPoolFees();
    this.ros = projection.getRos();
    this.stakeAmount = projection.getActiveStake();
  }
}
