package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolOwner;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOwnerRepository extends JpaRepository<PoolOwner, Long> {

  @Query(value = "SELECT sa.view FROM PoolOwner po "
      + "JOIN StakeAddress sa ON sa.id = po.stakeAddress.id "
      + "WHERE po.poolUpdate.id IN :poolUpdateId "
      + "ORDER BY po.id DESC")
  Set<String> getStakeKeyList(@Param("poolUpdateId") Set<Long> poolUpdateIds);
}
