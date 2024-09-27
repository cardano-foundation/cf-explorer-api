package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.ledgersync.AdaPots;

@Repository
public interface AdaPotsRepository extends JpaRepository<AdaPots, Long> {

  Boolean existsByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT ap FROM AdaPots ap WHERE ap.epochNo = :epochNo ORDER BY ap.slotNo DESC LIMIT 1")
  AdaPots findByEpochNo(@Param("epochNo") Integer epochNo);
}
