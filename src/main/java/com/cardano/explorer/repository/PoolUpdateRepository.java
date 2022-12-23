package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.PoolDetailUpdateProjection;
import com.sotatek.cardano.common.entity.PoolUpdate;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long> {

  @Query(value =
      "SELECT sa.view FROM PoolUpdate pu JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  List<String> findRewardAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.view FROM PoolOwner po JOIN PoolUpdate pu ON po.poolUpdate.id = pu.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  List<String> findOwnerAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT pu.pledge AS pledge, pu.margin AS margin, pu.fixedCost AS cost, ep.optimalPoolCount AS paramK, ap.utxo AS utxo "
          + "FROM PoolUpdate pu "
          + "JOIN EpochParam ep ON ep.epochNo = pu.activeEpochNo "
          + "JOIN AdaPots ap ON ap.epochNo = pu.activeEpochNo "
          + "WHERE pu.id = (SELECT max(pu.id) FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId) "
          + "AND pu.poolHash.id = :poolId")
  PoolDetailUpdateProjection findPoolUpdateByPoolId(@Param("poolId") Long poolId);
}
