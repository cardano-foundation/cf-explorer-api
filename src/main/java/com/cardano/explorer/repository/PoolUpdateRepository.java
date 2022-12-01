package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolUpdate;
import java.math.BigDecimal;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long> {

  @Query(value = "SELECT pu.pledge FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId ORDER BY pu.activeEpochNo DESC")
  List<BigDecimal> findPledgeByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.hashRaw FROM PoolUpdate pu JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId ORDER BY pu.activeEpochNo DESC")
  List<String> findRewardAccountByPool(@Param("poolId") Long poolId);

  @Query(value =
      "SELECT sa.hashRaw FROM PoolOwner po JOIN PoolUpdate pu ON po.poolUpdate.id = pu.id "
          + "JOIN StakeAddress sa ON po.stakeAddress.id = sa.id "
          + "WHERE pu.poolHash.id = :poolId ORDER BY pu.activeEpochNo DESC")
  List<String> findOwnerAccountByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT pu FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId ORDER BY pu.activeEpochNo DESC")
  List<PoolUpdate> findAllByPoolId(@Param("poolId") Long poolId);
}
