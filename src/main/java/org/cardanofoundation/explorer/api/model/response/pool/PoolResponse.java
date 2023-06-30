package org.cardanofoundation.explorer.api.model.response.pool;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
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

  private BigInteger poolSize;

  @JsonSerialize(using = PercentSerializer.class)
  private Double reward;

  @JsonSerialize(using = PercentSerializer.class)
  private Double feePercent;

  private BigInteger feeAmount;

  private BigInteger pledge;

  @JsonSerialize(using = PercentSerializer.class)
  private Double saturation;

  private BigDecimal stakeLimit;

  private Integer kParam;

  private BigInteger reserves;

  private Integer numberDelegators;

  private Integer epochBlock;

  private Integer lifetimeBlock;

  @JsonSerialize(using = PercentSerializer.class)
  private Double lifetimeRos;
}
