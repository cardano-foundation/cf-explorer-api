package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.model.response.BaseFilterResponse;
import java.io.Serializable;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PoolDetailListResponse implements Serializable {

  private BaseFilterResponse<PoolDetailEpochResponse> epoch;

  private PoolDetailAnalyticsResponse analytics;
}
