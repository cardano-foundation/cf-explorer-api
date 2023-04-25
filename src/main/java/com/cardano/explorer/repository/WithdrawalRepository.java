package com.cardano.explorer.repository;

import com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse;
import com.cardano.explorer.projection.StakeWithdrawalProjection;
import com.sotatek.cardano.common.entity.StakeAddress;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Withdrawal;
import com.sotatek.cardano.common.entity.Withdrawal_;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface WithdrawalRepository extends JpaRepository<Withdrawal, Long> {

  @EntityGraph(attributePaths = {Withdrawal_.ADDR})
  List<Withdrawal> findByTx(Tx tx);

  @Query("SELECT SUM(w.amount) FROM Withdrawal w "
      + " INNER JOIN StakeAddress stakeAddress ON w.addr.id = stakeAddress.id"
      + " WHERE stakeAddress.view = :stakeAddress")
  Optional<BigInteger> getRewardWithdrawnByStakeAddress(String stakeAddress);

  @Query("SELECT tx.hash as txHash,withdrawal.amount as amount, block.time as time, tx.fee as fee,"
      + " block.epochNo as epochNo"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " WHERE withdrawal.addr = :stakeKey AND tx.hash = :hash")
  Optional<StakeWithdrawalProjection> getWithdrawalByAddressAndTx(StakeAddress stakeKey, String hash);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, withdrawal.amount as amount"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON withdrawal.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeWithdrawalProjection> getWithdrawalByAddress(String stakeKey, Pageable pageable);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, withdrawal.amount as amount"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " WHERE withdrawal.addr = :stakeKey"
      + " AND (block.time >= :fromTime )"
      + " AND (block.time <= :toTime)"
      + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeWithdrawalProjection> getWithdrawalByAddress(StakeAddress stakeKey, String txHash,
      Timestamp fromTime, Timestamp toTime, Pageable pageable);

  @Query("SELECT sum(wd.amount) "
      + "FROM Withdrawal wd "
      + "WHERE wd.addr.id IN ( "
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
  BigInteger findWithdrawalStakeByPool(@Param("poolView") String poolView);

  @Query("SELECT new com.cardano.explorer.model.response.stake.lifecycle.StakeRewardResponse("
      + "block.epochNo, epoch.endTime, sum(withdrawal.amount))"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN Epoch epoch ON block.epochNo = epoch.no"
      + " WHERE withdrawal.addr = :stakeAddress"
      + " GROUP BY block.epochNo, epoch.endTime")
  List<StakeRewardResponse> findEpochWithdrawalByStake(StakeAddress stakeAddress);
}
