package org.cardanofoundation.explorer.api.repository;

import java.util.List;
import org.cardanofoundation.explorer.consumercommon.entity.EpochStakeCheckpoint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface EpochStakeCheckpointRepository extends JpaRepository<EpochStakeCheckpoint, Long> {

  @Query("SELECT COUNT(r.id) FROM EpochStakeCheckpoint r "
      + "WHERE r.stakeAddress IN :rewardAccounts AND r.epochCheckpoint = "
      + "(SELECT max(e.no) FROM Epoch e)")
  Integer checkEpochStakeByAccountsAndEpoch(@Param("rewardAccounts") List<String> rewardAccounts);
}
