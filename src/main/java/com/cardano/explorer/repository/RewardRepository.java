package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Reward;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface RewardRepository extends JpaRepository<Reward, Long> {

  @Query("SELECT SUM(r.amount) FROM Reward r "
      + " INNER JOIN StakeAddress stakeAddress ON r.addr.id = stakeAddress.id"
      + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
      + " AND stakeAddress.view = :stakeAddress")
  Optional<BigDecimal> getAvailableRewardByStakeAddress(String stakeAddress);

  @Query(value = "SELECT sum(rw.amount) FROM Reward rw WHERE rw.pool.id = :poolId AND rw.earnedEpoch = :epochNo")
  BigDecimal totalRewardStakeByEpochNoAndPool(@Param("epochNo") Long epochNo,
      @Param("poolId") Long poolId);
}
