package com.cardano.explorer.model.response.pool;

import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.PoolDetailDelegatorResponse;
import java.io.Serializable;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PoolDetailDelegatorsResponse implements Serializable {

  private BaseFilterResponse<PoolDetailDelegatorResponse> delegators;

  private Long totalEpoch;
}
