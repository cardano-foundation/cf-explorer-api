package org.cardanofoundation.explorer.api.repository;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.consumercommon.entity.RewardCheckpoint;

public interface RewardCheckpointRepository extends JpaRepository<RewardCheckpoint, Long> {

  @Query("SELECT CASE WHEN COUNT(r) > 0 THEN true ELSE false END FROM RewardCheckpoint r "
      + "WHERE r.stakeAddress = :stakeAddress AND r.epochCheckpoint = "
      + "(SELECT max(epoch.no) - 1 FROM Epoch epoch)")
  boolean checkRewardByStakeAddressAndEpoch(@Param("stakeAddress") String stakeAddress);

  @Query("SELECT COUNT(r.id) FROM RewardCheckpoint r "
      + "WHERE r.stakeAddress IN :rewardAccounts AND r.epochCheckpoint = "
      + "(SELECT max(e.no) - 1 FROM Epoch e)")
  Integer checkRewardByRewardAccountsAndEpoch(@Param("rewardAccounts") List<String> rewardAccounts);
}
