package com.cardano.explorer.repository;

import com.cardano.explorer.entity.EpochStake;
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

  @Query(value = "SELECT count(tx.blockId) FROM Tx tx WHERE tx.id in (SELECT sa.tx.id FROM EpochStake es JOIN StakeAddress sa ON es.addr.id = sa.id JOIN Tx tx ON sa.tx.id = tx.id WHERE es.pool.id = :poolId GROUP BY sa.tx.id)")
  Optional<Integer> countBlockByPoolId(@Param("poolId") Long poolId);
}
