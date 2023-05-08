package org.cardanofoundation.explorer.api.model.response.pool.lifecycle;

import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class RewardAllResponse {

  private String poolId;

  private String poolName;

  private String poolView;

  private BaseFilterResponse<RewardResponse> rewards;
}
