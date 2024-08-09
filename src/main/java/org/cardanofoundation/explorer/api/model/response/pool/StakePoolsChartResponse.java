package org.cardanofoundation.explorer.api.model.response.pool;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StakePoolsChartResponse {
  private Long registeredPool;
  private Long activePool;
}
