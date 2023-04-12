package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.EpochStakeProjection;
import com.cardano.explorer.model.response.stake.StakeAnalyticRewardResponse;
import com.sotatek.cardano.common.entity.Reward;
import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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

  @Query("SELECT sum(rw.amount) "
      + "FROM Reward rw "
      + "WHERE rw.spendableEpoch <= (SELECT max(epochNo) FROM Block) "
      + "AND rw.addr.id IN ( "
      + "SELECT DISTINCT d.address.id "
      + "FROM Delegation d "
      + "JOIN PoolHash ph ON ph.id = d.poolHash.id "
      + "JOIN StakeAddress sa ON sa.id = d.address.id "
      + "WHERE d.address.id  NOT IN ( "
      + "SELECT d1.address.id "
      + "FROM Delegation d1 "
      + "JOIN PoolHash ph ON ph.id = d1.poolHash.id "
      + "JOIN StakeAddress sa ON sa.id = d1.address.id "
      + "WHERE d1.address.id  = d.address.id "
      + "AND d1.id > d.id) "
      + "AND d.address.id IN ( "
      + "SELECT d.address.id "
      + "FROM Delegation d "
      + "JOIN PoolHash ph ON ph.id = d.poolHash.id "
      + "WHERE ph.view = :poolView) AND ph.view  = :poolView)")
  BigInteger findRewardStakeByPool(@Param("poolView") String poolView);

  @Query("SELECT new com.cardano.explorer.model.response.stake.StakeAnalyticRewardResponse"
      + " (rw.earnedEpoch , COALESCE(sum(rw.amount), 0))"
      + " FROM Reward rw"
      + " WHERE rw.spendableEpoch <= (SELECT max(no) FROM Epoch)"
      + " AND rw.addr = (SELECT sa FROM StakeAddress sa WHERE sa.view = :stakeAddress)"
      + " GROUP BY rw.earnedEpoch")
  List<StakeAnalyticRewardResponse> findRewardByStake(String stakeAddress);
}
