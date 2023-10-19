package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.*;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolActiveStakeProjection;
import org.cardanofoundation.explorer.api.projection.StakeRewardProjection;
import org.cardanofoundation.explorer.consumercommon.entity.PoolHash;
import org.cardanofoundation.explorer.consumercommon.entity.Reward;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.enumeration.RewardType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.security.core.parameters.P;
import org.springframework.stereotype.Repository;

@Repository
public interface RewardRepository extends JpaRepository<Reward, Long> {

  @Query("SELECT SUM(r.amount) FROM Reward r "
      + " INNER JOIN StakeAddress stakeAddress ON r.addr.id = stakeAddress.id"
      + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
      + " AND stakeAddress.view = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddress(@Param("stakeAddress") String stakeAddress);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, sum(rw.amount) AS totalStake FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND rw.earnedEpoch IN :epochNo "
      + "GROUP BY rw.earnedEpoch")
  List<PoolActiveStakeProjection> totalRewardStakeByEpochNoAndPool(@Param("epochNo") Set<Long> epochNo,
      @Param("poolId") Long poolId);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse"
          + " (rw.earnedEpoch , COALESCE(sum(rw.amount), 0))"
          + " FROM Reward rw"
          + " WHERE rw.spendableEpoch <= (SELECT max(no) FROM Epoch)"
          + " AND rw.addr = (SELECT sa FROM StakeAddress sa WHERE sa.view = :stakeAddress)"
          + " GROUP BY rw.earnedEpoch")
  List<StakeAnalyticRewardResponse> findRewardByStake(@Param("stakeAddress") String stakeAddress);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.startTime, rw.amount, rw.type)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress"
      + " AND (epoch.startTime >= :fromDate )"
      + " AND (epoch.startTime <= :toDate )"
      + " AND (rw.type = :rewardType OR :rewardType IS NULL)")
  Page<StakeRewardResponse> findRewardByStake(@Param("stakeAddress") StakeAddress stakeAddress,
                                              @Param("fromDate") Timestamp fromDate,
                                              @Param("toDate") Timestamp toDate,
                                              @Param("rewardType") RewardType rewardType,
                                              Pageable pageable);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.startTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr = :stakeAddress")
  List<StakeRewardResponse> findRewardByStake(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query("SELECT SUM(r.amount) FROM Reward r "
      + " WHERE r.spendableEpoch <= :epoch"
      + " AND r.addr = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddressAndEpoch(
      @Param("stakeAddress") StakeAddress stakeAddress, @Param("epoch") Integer epoch);


  @Query(value =
      "SELECT rw.earnedEpoch AS epochNo, e.startTime AS time, rw.amount AS amount, sa.view AS address "
          + "FROM Reward rw "
          + "JOIN PoolHash ph ON rw.pool.id = ph.id "
          + "JOIN StakeAddress sa ON rw.addr.id = sa.id "
          + "JOIN Epoch e ON rw.spendableEpoch = e.no "
          + "WHERE (ph.view  = :poolViewOrHash OR ph.hashRaw = :poolViewOrHash) AND rw.type = 'leader' ")
  Page<LifeCycleRewardProjection> getRewardInfoByPool(@Param("poolViewOrHash") String poolViewOrHash,
                                                      Pageable pageable);

  @Query(value =
      "SELECT rw.earnedEpoch AS epochNo, e.startTime AS time, rw.amount AS amount, sa.view AS address "
          + "FROM Reward rw "
          + "JOIN PoolHash ph ON rw.pool.id = ph.id "
          + "JOIN StakeAddress sa ON rw.addr.id = sa.id "
          + "JOIN Epoch e ON rw.spendableEpoch = e.no "
          + "WHERE (ph.view  = :poolViewOrHash OR ph.hashRaw = :poolViewOrHash) AND rw.type = 'leader' "
          + "AND (rw.earnedEpoch >= :beginEpoch) "
          + "AND (rw.earnedEpoch <= :endEpoch)")
  Page<LifeCycleRewardProjection> getRewardInfoByPoolFiler(@Param("poolViewOrHash") String poolViewOrHash,
                                                      @Param("beginEpoch") Integer beginEpoch,
                                                      @Param("endEpoch") Integer endEpoch,
                                                      Pageable pageable);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, rw.amount AS amount "
      + "FROM Reward rw "
      + "JOIN PoolHash ph ON rw.pool.id = ph.id "
      + "WHERE ph.view = :poolView AND rw.type = 'refund' AND rw.earnedEpoch IN :epochNos")
  List<EpochRewardProjection> getRewardRefundByEpoch(@Param("poolView") String poolView,
                                                     @Param("epochNos") Set<Integer> epochNos);

  @Query(value = "SELECT sum(rw.amount) FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND (rw.type = 'leader' or rw.type = 'member') AND rw.spendableEpoch = (SELECT max(e.no) FROM Epoch e)")
  BigInteger getPoolRewardByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT sum(rw.amount) "
      + "FROM Reward rw "
      + "JOIN PoolHash ph ON rw.pool.id = ph.id "
      + "WHERE ph.view  = :poolView AND rw.type = 'leader' ")
  BigInteger getTotalRewardByPool(@Param("poolView") String poolView);

  Boolean existsByAddr(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
      + "(rw.spendableEpoch, epoch.startTime, rw.amount)"
      + " FROM Reward rw"
      + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
      + " WHERE rw.addr.view = :stakeKey"
      + " AND (epoch.startTime >= :fromDate )"
      + " AND (epoch.startTime <= :toDate )")
  Page<StakeRewardResponse> findRewardByStake(@Param("stakeKey") String stakeKey,
                                              @Param("fromDate") Timestamp fromDate,
                                              @Param("toDate") Timestamp toDate,
                                              Pageable pageable);

  @Query(value = "SELECT ph.view AS view, sum(rw.amount) AS amount "
      + "FROM Reward rw "
      + "JOIN PoolHash ph ON rw.pool.id = ph.id "
      + "WHERE ph.view IN :poolViews AND rw.type = 'leader' AND rw.spendableEpoch = :epochNo "
      + "GROUP BY ph.view")
  List<PoolAmountProjection> getOperatorRewardByPoolList(@Param("poolViews") Set<String> poolViews,
      @Param("epochNo") Integer epochNo);

  @Query(value = "SELECT rw.pool.id AS poolId, sum(rw.amount) AS amount "
      + "FROM Reward rw "
      + "WHERE rw.pool.id IN :poolIds AND rw.type IN ('leader', 'member') AND rw.spendableEpoch = :epochNo "
      + "GROUP BY rw.pool.id")
  List<PoolAmountProjection> getPoolRewardByPoolList(@Param("poolIds") Set<Long> poolIds, @Param("epochNo") Integer epochNo);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, sum(rw.amount) AS amount FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND rw.type = 'member' "
      + "AND rw.earnedEpoch IN :epochNos "
      + "GROUP BY rw.earnedEpoch")
  List<EpochRewardProjection> getDelegatorRewardByPool(@Param("poolId") Long poolId,
      @Param("epochNos") Set<Integer> epochNos);

  @Query(value = "SELECT rw.earnedEpoch AS epochNo, sum(rw.amount) AS amount FROM Reward rw "
      + "WHERE rw.pool.id = :poolId AND rw.type IN ('leader', 'member') "
      + "AND rw.earnedEpoch IN :epochNos "
      + "GROUP BY rw.earnedEpoch")
  List<EpochRewardProjection> getPoolRewardByPool(@Param("poolId") Long poolId,
      @Param("epochNos") Set<Integer> epochNos);

  Boolean existsByPoolAndType(@Param("pool") PoolHash pool, @Param("type") RewardType type);

  @Query("SELECT SUM(r.amount) as amount, r.stakeAddressId as stakeAddressId"
      + " FROM Reward r "
      + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
      + " AND r.stakeAddressId IN :stakeAddressIds"
      + " GROUP BY r.stakeAddressId")
  List<StakeRewardProjection> getTotalRewardByStakeAddressIn(@Param("stakeAddressIds") Collection<Long> stakeAddressIds);

  @Query("SELECT reward.type FROM Reward reward "
      + "JOIN StakeAddress stakeAddress ON stakeAddress.id = reward.stakeAddressId "
      + "WHERE stakeAddress.view = :stakeView")
  Set<RewardType> getAllRewardTypeOfAStakeKey(@Param("stakeView") String stakeView);

  @Query("SELECT COALESCE(SUM(r.amount), 0) FROM Reward r "
      + " INNER JOIN StakeAddress stakeAddress ON r.addr.id = stakeAddress.id"
      + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
      + " AND stakeAddress.view IN :addressList")
  BigInteger getAvailableRewardByAddressList(@Param("addressList") List<String> addressList);
}
