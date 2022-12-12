package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolStake;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolStakeRepository extends JpaRepository<PoolStake, Long> {

  @Query(value = "SELECT ps.amount FROM PoolStake ps WHERE ps.pool.id = :poolId")
  Optional<BigDecimal> findTotalStakeByPoolId(@Param("poolId") Long poolId);
}
