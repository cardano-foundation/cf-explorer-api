package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.pool.projection.EpochStakeProjection;
import com.cardano.explorer.model.response.pool.projection.LifeCycleRewardProjection;
import com.cardano.explorer.model.response.pool.projection.PoolAmountProjection;
import com.cardano.explorer.model.response.pool.projection.EpochRewardProjection;
import com.cardano.explorer.model.response.stake.StakeAnalyticRewardResponse;
import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.sotatek.cardano.common.entity.Reward;
import com.sotatek.cardano.common.entity.StakeAddress;
import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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

  @Query("SELECT new com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.endTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress")
  Page<StakeRewardResponse> findRewardByStake(StakeAddress stakeAddress, Pageable pageable);

  @Query("SELECT new com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.endTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress")
  List<StakeRewardResponse> findRewardByStake(StakeAddress stakeAddress);

  @Query("SELECT SUM(r.amount) FROM Reward r "
      + " WHERE r.spendableEpoch <= :epoch"
      + " AND r.addr = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddressAndEpoch(StakeAddress stakeAddress, Integer epoch);


  @Query(value = "SELECT rw.earnedEpoch AS epochNo, e.startTime AS time, rw.amount AS amount, sa.view AS address "
      + "FROM Reward rw "
      + "JOIN PoolHash ph ON rw.pool.id = ph.id "
      + "JOIN StakeAddress sa ON rw.addr.id = sa.id "
      + "JOIN Epoch e ON rw.spendableEpoch = e.no "
      + "WHERE ph.view  = :poolView AND rw.type = 'leader' "
      + "ORDER BY rw.earnedEpoch DESC")
  Page<LifeCycleRewardProjection> getRewardInfoByPool(@Param("poolView") String poolView, Pageable pageable);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, rw.amount AS amount "
      + "FROM Reward rw "
      + "JOIN PoolHash ph ON rw.pool.id = ph.id "
      + "WHERE ph.view = :poolView AND rw.type = 'refund' AND rw.earnedEpoch IN :epochNos")
  List<EpochRewardProjection> getRewardRefundByEpoch(@Param("poolView") String poolView, @Param("epochNos") Set<Integer> epochNos);

  @Query(value = "SELECT sum(rw.amount) FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND (rw.type = 'leader' or rw.type = 'member') AND rw.spendableEpoch = (SELECT max(e.no) FROM Epoch e)")
  BigInteger getPoolRewardByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT rw.pool.id AS poolId, sum(rw.amount) AS amount "
      + "FROM Reward rw "
      + "WHERE rw.pool.id IN :poolIds AND (rw.type = 'leader' OR rw.type = 'member') AND rw.spendableEpoch = (SELECT max(e.no) FROM Epoch e) "
      + "GROUP BY rw.pool.id")
  List<PoolAmountProjection> getPoolRewardByPoolList(@Param("poolIds") Set<Long> poolIds);

  @Query(value = "SELECT rw.spendableEpoch AS epochNo, sum(rw.amount) AS amount FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND rw.type = 'member' "
      + "AND rw.spendableEpoch IN :epochNos "
      + "GROUP BY rw.spendableEpoch")
  List<EpochRewardProjection> getDelegatorRewardByPool(@Param("poolId") Long poolId, @Param("epochNos") Set<Integer> epochNos);
}
