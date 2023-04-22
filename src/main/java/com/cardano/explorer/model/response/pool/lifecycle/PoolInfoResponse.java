package com.cardano.explorer.model.response.pool.lifecycle;

import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PoolInfoResponse implements Serializable {

  String poolId;

  String poolName;

  String poolView;

  List<String> stakeKeys;
}
