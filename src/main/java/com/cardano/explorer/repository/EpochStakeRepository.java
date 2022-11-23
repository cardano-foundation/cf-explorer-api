package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.EpochStake;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface EpochStakeRepository extends JpaRepository<EpochStake, Long> {

  @Query(value = "SELECT sum(amount) FROM EpochStake WHERE epochNo = :epochNo")
  Optional<BigDecimal> totalValueStakeByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(value = "SELECT count(bk.id) from Block bk WHERE bk.epochNo in (SELECT es.epochNo FROM PoolHash ph JOIN EpochStake es on ph.id = es.pool.id WHERE ph.id = :poolId GROUP BY es.epochNo)")
  Integer countBlockByPoolId(@Param("poolId") Long poolId);

  @Query(value = "select count(bk.id) from Block bk WHERE bk.epochNo = (SELECT max(ep.no) FROM Epoch ep)")
  Integer countBlockByCurrentEpoch();
}
