package com.cardano.explorer.repository;

import com.cardano.explorer.entity.PoolHash;
import com.cardano.explorer.entity.PoolUpdate;
import com.cardano.explorer.repository.custom.CustomPoolUpdateRepository;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolUpdateRepository extends JpaRepository<PoolUpdate, Long>,
    CustomPoolUpdateRepository {

  @Query(value = "SELECT sum(pu.pledge) FROM PoolUpdate pu WHERE pu.poolHash.id = :poolId")
  Optional<BigDecimal> sumPledgeByPool(@Param(("poolId")) Long poolId);

  Optional<PoolUpdate> findFirstByPoolHash(PoolHash poolHash);
}
