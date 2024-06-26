package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.ledgersync.EpochStakeCheckpoint;

@Repository
public interface EpochStakeCheckpointRepository extends JpaRepository<EpochStakeCheckpoint, Long> {

  @Query(
      "SELECT COUNT(r.id) FROM EpochStakeCheckpoint r "
          + "WHERE r.stakeAddress IN :rewardAccounts AND r.epochCheckpoint = "
          + "(SELECT max(e.no) FROM Epoch e)")
  Integer checkEpochStakeByAccountsAndEpoch(@Param("rewardAccounts") List<String> rewardAccounts);
}
