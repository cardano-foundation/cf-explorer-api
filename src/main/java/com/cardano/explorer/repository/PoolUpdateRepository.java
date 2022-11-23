package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolUpdate;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long> {

  @Query(value = "SELECT sum(pu.pledge) FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId")
  BigDecimal sumPledgeByPool(@Param("poolId") Long poolId);

  PoolUpdate findFirstByPoolHash(PoolHash poolHash);

  @Query(value = "SELECT sa.view FROM PoolUpdate pu JOIN StakeAddress sa ON pu.rewardAddr.id = sa.id WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  String findRewardAccountByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT sa.view FROM PoolOwner po JOIN PoolUpdate pu ON po.poolUpdate.id = pu.id JOIN StakeAddress sa ON po.stakeAddress.id = sa.id WHERE pu.poolHash.id = :poolId GROUP BY sa.view")
  String findOwnerAccountByPool(@Param("poolId") Long poolId);
}
