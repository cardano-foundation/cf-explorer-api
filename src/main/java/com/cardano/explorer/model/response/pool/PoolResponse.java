package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.json.serialize.PercentSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PoolResponse implements Serializable {

  private String poolId;

  private String poolName;

  private BigDecimal poolSize;

  private Double reward;

  @JsonSerialize(using = PercentSerializer.class)
  private Double feePercent;

  private BigDecimal feeAmount;

  private BigDecimal pledge;

  @JsonSerialize(using = PercentSerializer.class)
  private Double saturation;

}
