package org.cardanofoundation.explorer.api.model.response.pool;

import java.io.Serializable;
import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import org.cardanofoundation.explorer.api.json.serialize.PercentSerializer;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PoolResponse implements Serializable {

  private Long id;

  private String poolId;

  private String poolName;

  private String tickerName;

  private BigInteger poolSize;
  private BigInteger pledge;

  @JsonSerialize(using = PercentSerializer.class)
  private Double saturation;

  private Integer epochBlock;

  private Integer lifetimeBlock;

  private Double votingPower;

  private Double governanceParticipationRate;

  private boolean retired;
}
