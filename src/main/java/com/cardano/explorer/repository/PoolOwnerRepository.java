package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.BlockOwnerProjection;
import com.sotatek.cardano.common.entity.PoolOwner;
import java.util.List;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOwnerRepository extends JpaRepository<PoolOwner, Long> {

  @Query(value = "SELECT bk.id AS blockId, sa.view AS address FROM Block bk "
      + "JOIN SlotLeader sl ON sl.id = bk.slotLeader.id "
      + "JOIN PoolHash ph ON ph.id = sl.poolHash.id "
      + "JOIN PoolUpdate pu ON pu.poolHash.id = ph.id "
      + "JOIN PoolOwner po ON po.poolUpdate = pu.id "
      + "JOIN StakeAddress sa ON sa.id = po.stakeAddress.id "
      + "WHERE bk.id IN :blockIds "
      + "GROUP BY bk.id, sa.view")
  List<BlockOwnerProjection> getStakeKeyList(@Param("blockIds") Set<Long> blockIds);
}
