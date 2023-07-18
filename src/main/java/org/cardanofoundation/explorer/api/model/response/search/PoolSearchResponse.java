package org.cardanofoundation.explorer.api.model.response.search;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class PoolSearchResponse {
  private String name;
  private String poolId;
  private String icon;
}
