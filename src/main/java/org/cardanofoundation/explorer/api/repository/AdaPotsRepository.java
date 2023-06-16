package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.AdaPots;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface AdaPotsRepository extends JpaRepository<AdaPots, Long> {

  Boolean existsByEpochNo(@Param("epochNo") Integer epochNo);
}
