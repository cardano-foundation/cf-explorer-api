package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.Committee;

public interface CommitteeRepository extends JpaRepository<Committee, Integer> {

  @Query("""
    SELECT c.threshold FROM Committee c ORDER BY c.epoch DESC LIMIT 1
    """)
  Optional<Double> getLatestCCThreshold();

}
