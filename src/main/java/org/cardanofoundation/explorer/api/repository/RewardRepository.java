package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochStakeProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Reward;
import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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
  Optional<BigInteger> getAvailableRewardByStakeAddress(String stakeAddress);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, sum(rw.amount) AS totalStake FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND rw.earnedEpoch IN :epochNo "
      + "GROUP BY rw.earnedEpoch")
  List<EpochStakeProjection> totalRewardStakeByEpochNoAndPool(@Param("epochNo") Set<Long> epochNo,
      @Param("poolId") Long poolId);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.endTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress")
  List<StakeRewardResponse> findRewardByStake(StakeAddress stakeAddress);

  @Query("SELECT SUM(r.amount) FROM Reward r "
      + " WHERE r.spendableEpoch <= :epoch"
      + " AND r.addr = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddressAndEpoch(StakeAddress stakeAddress, Integer epoch);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.endTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress")
  Page<StakeRewardResponse> findRewardByStake(StakeAddress stakeAddress, Pageable pageable);
}
