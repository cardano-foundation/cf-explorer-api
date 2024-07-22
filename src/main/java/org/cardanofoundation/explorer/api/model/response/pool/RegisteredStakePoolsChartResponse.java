package org.cardanofoundation.explorer.api.model.response.pool;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class RegisteredStakePoolsChartResponse {
  private Long registered;
  private Long active;
}
