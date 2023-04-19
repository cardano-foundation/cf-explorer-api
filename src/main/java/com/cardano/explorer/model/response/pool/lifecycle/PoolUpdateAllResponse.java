package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.util.List;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PoolUpdateAllResponse implements Serializable {

  private String poolId;

  private String poolName;

  private String poolView;

  private List<PoolUpdateResponse> poolUpdates;
}
