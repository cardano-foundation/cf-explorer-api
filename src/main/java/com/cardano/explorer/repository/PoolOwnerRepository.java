package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolOwnerProjection;
import com.sotatek.cardano.common.entity.PoolOwner;
import java.util.List;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOwnerRepository extends JpaRepository<PoolOwner, Long> {

  @Query(value = "SELECT pu.poolHash.id AS poolId, sa.view AS address "
      + "FROM PoolUpdate pu "
      + "JOIN PoolOwner po ON po.poolUpdate.id  = pu.id  "
      + "JOIN StakeAddress sa ON sa.id = po.stakeAddress.id "
      + "WHERE pu.poolHash.id IN :poolIds")
  List<PoolOwnerProjection> getStakeKeyList(@Param("poolIds") Set<Long> poolIds);
}
