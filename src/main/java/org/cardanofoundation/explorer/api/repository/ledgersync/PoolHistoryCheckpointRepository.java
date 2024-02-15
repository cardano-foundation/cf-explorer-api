package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.ledgersync.PoolHistoryCheckpoint;

@Repository
public interface PoolHistoryCheckpointRepository
    extends JpaRepository<PoolHistoryCheckpoint, Long> {

  @Query(
      value =
          "SELECT COUNT(cp.id) FROM PoolHistoryCheckpoint cp "
              + "WHERE cp.view IN :poolViews AND cp.epochCheckpoint = "
              + "(SELECT max(e.no) - 1 FROM Epoch e) AND cp.isSpendableReward = TRUE")
  Integer checkRewardByPoolViewAndEpoch(@Param("poolViews") Set<String> poolViews);

  @Query(
      value =
          "SELECT ph.view FROM PoolHash ph WHERE ph.view NOT IN ( "
              + "SELECT pc.view FROM PoolHistoryCheckpoint pc "
              + "WHERE pc.epochCheckpoint = (SELECT max(e.no) - 1 FROM Epoch e) AND pc.isSpendableReward = TRUE AND pc.view IN :poolViews ) "
              + "AND ph.view IN :poolViews ")
  Set<String> checkPoolHistoryByPoolViewAndEpoch(@Param("poolViews") Set<String> poolViews);
}
