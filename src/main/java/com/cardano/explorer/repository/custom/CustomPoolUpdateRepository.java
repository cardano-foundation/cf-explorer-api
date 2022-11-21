package com.cardano.explorer.repository.custom;

import com.cardano.explorer.entity.PoolUpdate;

public interface CustomPoolUpdateRepository extends CustomRepository<PoolUpdate> {

  String findRewardAccountByPool(Long poolId);

  String findOwnerAccountByPool(Long poolId);
}
