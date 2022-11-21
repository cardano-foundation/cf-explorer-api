package com.cardano.explorer.repository.custom;

import com.cardano.explorer.entity.PoolHash;
import com.cardano.explorer.model.request.DelegationFilterRequest;
import java.util.List;

public interface CustomPoolHashRepository extends CustomRepository<PoolHash> {

  List<Long> findAllPoolHashId(DelegationFilterRequest delegationFilterRequest);
}
