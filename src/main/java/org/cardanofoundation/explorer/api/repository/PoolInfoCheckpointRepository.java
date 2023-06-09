package org.cardanofoundation.explorer.api.repository;

import java.util.Set;
import org.cardanofoundation.explorer.consumercommon.entity.PoolInfoCheckpoint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolInfoCheckpointRepository extends JpaRepository<PoolInfoCheckpoint, Long> {

  @Query("SELECT COUNT(cp.id) FROM PoolInfoCheckpoint cp "
      + "WHERE cp.view IN :poolViews AND cp.epochCheckpoint = "
      + "(SELECT max(e.no) FROM Epoch e)")
  Integer checkRewardByPoolViewAndEpoch(@Param("poolViews") Set<String> poolViews);
}
