package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.StakeWithdrawalProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal_;
import org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collection;
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
  List<Withdrawal> findByTx(@Param("tx") Tx tx);

  @Query("SELECT SUM(w.amount) FROM Withdrawal w "
      + " INNER JOIN StakeAddress stakeAddress ON w.addr.id = stakeAddress.id"
      + " WHERE stakeAddress.view = :stakeAddress")
  Optional<BigInteger> getRewardWithdrawnByStakeAddress(@Param("stakeAddress") String stakeAddress);

  @Query("SELECT tx.hash as txHash,withdrawal.amount as amount, block.time as time, tx.fee as fee,"
      + " block.epochNo as epochNo, tx.id as txId"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " WHERE withdrawal.addr = :stakeKey AND tx.hash = :hash")
  Optional<StakeWithdrawalProjection> getWithdrawalByAddressAndTx(@Param("stakeKey") StakeAddress stakeKey,
                                                                  @Param("hash") String hash);

  @Query("SELECT withdrawal.tx.id"
      + " FROM Withdrawal withdrawal"
      + " WHERE withdrawal.addr = :stakeKey AND withdrawal.tx.id IN :txIds")
  List<Long> getWithdrawalByAddressAndTxIn(@Param("stakeKey") StakeAddress stakeKey,
                                           @Param("txIds") List<Long> txIds);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " block.blockNo as blockNo, block.epochNo as epochNo, withdrawal.amount as amount"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN StakeAddress stake ON withdrawal.addr = stake"
      + " WHERE stake.view = :stakeKey"
      + " ORDER BY block.blockNo DESC, tx.blockIndex DESC")
  Page<StakeWithdrawalProjection> getWithdrawalByAddress(@Param("stakeKey") String stakeKey,
                                                         Pageable pageable);

  @Query("SELECT tx.hash as txHash, block.time as time, block.epochSlotNo as epochSlotNo,"
      + " tx.fee as fee, block.blockNo as blockNo, block.epochNo as epochNo,"
      + " withdrawal.amount as amount"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " WHERE withdrawal.addr = :stakeKey"
      + " AND (block.time >= :fromTime )"
      + " AND (block.time <= :toTime)"
      + " AND ( :txHash IS NULL OR tx.hash = :txHash)")
  Page<StakeWithdrawalProjection> getWithdrawalByAddress(@Param("stakeKey") StakeAddress stakeKey,
                                                         @Param("txHash") String txHash,
                                                         @Param("fromTime") Timestamp fromTime,
                                                         @Param("toTime") Timestamp toTime,
                                                         Pageable pageable);

  @Query("SELECT new org.cardanofoundation.explorer.api.model.response.stake.lifecycle.StakeRewardResponse("
      + "block.epochNo, epoch.endTime, sum(withdrawal.amount))"
      + " FROM Withdrawal withdrawal"
      + " INNER JOIN Tx tx ON withdrawal.tx = tx"
      + " INNER JOIN Block block ON tx.block = block"
      + " INNER JOIN Epoch epoch ON block.epochNo = epoch.no"
      + " WHERE withdrawal.addr = :stakeAddress"
      + " GROUP BY block.epochNo, epoch.endTime")
  List<StakeRewardResponse> findEpochWithdrawalByStake(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query("SELECT sum(w.amount) FROM Withdrawal w"
      + " WHERE w.addr = :stakeAddress AND w.tx.id < :txId")
  Optional<BigInteger> sumByAddrAndTx(@Param("stakeAddress") StakeAddress stakeAddress,
                                      @Param("txId") Long txId);

  Boolean existsByAddr(@Param("stakeAddress") StakeAddress stakeAddress);

  @Query("SELECT w.stakeAddressId as stakeAddressId, SUM(w.amount) as amount" +
      " FROM Withdrawal w" +
      " WHERE w.stakeAddressId IN :stakeIds" +
      " GROUP BY w.stakeAddressId")
  List<StakeWithdrawalProjection> getRewardWithdrawnByAddrIn(@Param("stakeIds") Collection<Long> stakeIds);
}
