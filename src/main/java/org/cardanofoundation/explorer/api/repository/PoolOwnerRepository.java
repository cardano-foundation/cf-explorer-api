package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolOwnerProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolOwner;
import java.util.List;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOwnerRepository extends JpaRepository<PoolOwner, Long> {

  @Query(value = "SELECT ph.id AS poolId, sa.view AS address "
      + "FROM PoolHash ph "
      + "JOIN PoolUpdate pu ON ph.id = pu.poolHash.id AND pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE ph.id = pu.poolHash.id) "
      + "JOIN PoolOwner po ON po.poolUpdate.id  = pu.id  "
      + "JOIN StakeAddress sa ON sa.id = po.stakeAddress.id "
      + "WHERE ph.id IN :poolIds "
      + "GROUP BY ph.id, sa.view ")
  List<PoolOwnerProjection> getStakeKeyList(@Param("poolIds") Set<Long> poolIds);
}
