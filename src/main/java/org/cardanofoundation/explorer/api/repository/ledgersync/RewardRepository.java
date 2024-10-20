package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.*;
import org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import org.cardanofoundation.explorer.common.entity.enumeration.RewardType;
import org.cardanofoundation.explorer.common.entity.ledgersync.PoolHash;
import org.cardanofoundation.explorer.common.entity.ledgersync.Reward;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

@Repository
public interface RewardRepository extends JpaRepository<Reward, Long> {

  @Query(
      "SELECT SUM(r.amount) FROM Reward r "
          + " INNER JOIN StakeAddress stakeAddress ON r.addr.id = stakeAddress.id"
          + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
          + " AND stakeAddress.view = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddress(@Param("stakeAddress") String stakeAddress);

  @Query(
      "SELECT new org.cardanofoundation.explorer.api.model.response.stake.StakeAnalyticRewardResponse"
          + " (rw.earnedEpoch , COALESCE(sum(rw.amount), 0))"
          + " FROM Reward rw"
          + " WHERE rw.spendableEpoch <= (SELECT max(no) FROM Epoch)"
          + " AND rw.addr = (SELECT sa FROM StakeAddress sa WHERE sa.view = :stakeAddress)"
          + " GROUP BY rw.earnedEpoch")
  List<StakeAnalyticRewardResponse> findRewardByStake(@Param("stakeAddress") String stakeAddress);

  @Query(
      "SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
          + "(rw.spendableEpoch, epoch.startTime, rw.amount, rw.type, ph.view, ph.hashRaw)"
          + " FROM Epoch epoch"
          + " INNER JOIN Reward rw ON epoch.no = rw.spendableEpoch"
          + " LEFT JOIN PoolHash ph ON rw.pool.id = ph.id"
          + " WHERE rw.addr = :stakeAddress"
          + " AND (epoch.startTime >= :fromDate )"
          + " AND (epoch.startTime <= :toDate )"
          + " AND (rw.type = :rewardType OR :rewardType IS NULL)")
  Page<StakeRewardResponse> findRewardByStake(
      @Param("stakeAddress") StakeAddress stakeAddress,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      @Param("rewardType") RewardType rewardType,
      Pageable pageable);

  @Query(
      "SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
          + "(rw.spendableEpoch, epoch.startTime, rw.amount, rw.type, ph.view, ph.hashRaw)"
          + " FROM Reward rw"
          + " INNER JOIN Epoch epoch ON rw.spendableEpoch = epoch.no"
          + " LEFT JOIN PoolHash ph ON rw.pool.id = ph.id"
          + " WHERE rw.addr = :stakeAddress")
  List<StakeRewardResponse> findRewardByStake(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(
      "SELECT SUM(r.amount) FROM Reward r "
          + " WHERE r.spendableEpoch <= :epoch"
          + " AND r.addr = :stakeAddress")
  Optional<BigInteger> getAvailableRewardByStakeAddressAndEpoch(
      @Param("stakeAddress") StakeAddress stakeAddress, @Param("epoch") Integer epoch);

  @Query(
      value =
          "SELECT rw.earnedEpoch AS epochNo, e.startTime AS time, rw.amount AS amount, sa.view AS address "
              + "FROM Reward rw "
              + "JOIN PoolHash ph ON rw.pool.id = ph.id "
              + "JOIN StakeAddress sa ON rw.addr.id = sa.id "
              + "JOIN Epoch e ON rw.spendableEpoch = e.no "
              + "WHERE (ph.view  = :poolViewOrHash OR ph.hashRaw = :poolViewOrHash) AND rw.type = 'leader' ")
  Page<LifeCycleRewardProjection> getRewardInfoByPool(
      @Param("poolViewOrHash") String poolViewOrHash, Pageable pageable);

  @Query(
      value =
          "SELECT rw.earnedEpoch AS epochNo, e.startTime AS time, rw.amount AS amount, sa.view AS address "
              + "FROM Reward rw "
              + "JOIN PoolHash ph ON rw.pool.id = ph.id "
              + "JOIN StakeAddress sa ON rw.addr.id = sa.id "
              + "JOIN Epoch e ON rw.spendableEpoch = e.no "
              + "WHERE (ph.view  = :poolViewOrHash OR ph.hashRaw = :poolViewOrHash) AND rw.type = 'leader' "
              + "AND (rw.earnedEpoch >= :beginEpoch) "
              + "AND (rw.earnedEpoch <= :endEpoch)")
  Page<LifeCycleRewardProjection> getRewardInfoByPoolFiler(
      @Param("poolViewOrHash") String poolViewOrHash,
      @Param("beginEpoch") Integer beginEpoch,
      @Param("endEpoch") Integer endEpoch,
      Pageable pageable);

  @Query(
      value =
          "SELECT rw.earnedEpoch AS epochNo, rw.amount AS amount "
              + "FROM Reward rw "
              + "JOIN PoolHash ph ON rw.pool.id = ph.id "
              + "WHERE ph.view = :poolView AND rw.type = 'refund' AND rw.earnedEpoch IN :epochNos")
  List<EpochRewardProjection> getRewardRefundByEpoch(
      @Param("poolView") String poolView, @Param("epochNos") Set<Integer> epochNos);

  @Query(
      value =
          "SELECT sum(rw.amount) FROM Reward rw "
              + "WHERE rw.pool.id = :poolId AND (rw.type = 'leader' or rw.type = 'member') AND rw.spendableEpoch = (SELECT max(e.no) FROM Epoch e)")
  BigInteger getPoolRewardByPool(@Param("poolId") Long poolId);

  @Query(
      value =
          "SELECT sum(rw.amount) "
              + "FROM Reward rw "
              + "JOIN PoolHash ph ON rw.pool.id = ph.id "
              + "WHERE (ph.view  = :poolView OR ph.hashRaw = :poolView) AND rw.type = 'leader' ")
  BigInteger getTotalRewardByPool(@Param("poolView") String poolView);

  Boolean existsByAddr(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query(
      "SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse"
          + "(rw.spendableEpoch, epoch.startTime, rw.amount)"
          + " FROM Epoch epoch"
          + " INNER JOIN Reward rw ON rw.spendableEpoch = epoch.no"
          + " WHERE rw.addr.view = :stakeKey"
          + " AND (epoch.startTime >= :fromDate )"
          + " AND (epoch.startTime <= :toDate )")
  Page<StakeRewardResponse> findRewardByStake(
      @Param("stakeKey") String stakeKey,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      Pageable pageable);

  Boolean existsByPoolAndType(@Param("pool") PoolHash pool, @Param("type") RewardType type);

  @Query(
      "SELECT reward.type FROM Reward reward "
          + "JOIN StakeAddress stakeAddress ON stakeAddress.id = reward.stakeAddressId "
          + "WHERE stakeAddress.view = :stakeView")
  Set<RewardType> getAllRewardTypeOfAStakeKey(@Param("stakeView") String stakeView);

  @Query(
      "SELECT COALESCE(SUM(r.amount), 0) FROM Reward r "
          + " INNER JOIN StakeAddress stakeAddress ON r.addr.id = stakeAddress.id"
          + " WHERE r.spendableEpoch <= (SELECT max(no) FROM Epoch)"
          + " AND stakeAddress.view IN :addressList")
  BigInteger getAvailableRewardByAddressList(@Param("addressList") List<String> addressList);

  @Query(
      "SELECT COALESCE(SUM(r.amount), 0) FROM Reward r "
          + " WHERE r.addr = :stakeAddress"
          + " AND r.type = :type")
  Optional<BigInteger> getTotalRewardByStakeAddressAndType(
      @Param("stakeAddress") StakeAddress stakeAddress, @Param("type") RewardType type);
}
