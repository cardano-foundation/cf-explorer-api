package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.compositeKey.CommitteeRegistrationId;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeRegistration;

public interface CommitteeRegistrationRepository
    extends JpaRepository<CommitteeRegistration, CommitteeRegistrationId> {
  @Query(
      value =
          "SELECT COUNT(cr) FROM CommitteeRegistration cr "
              + "JOIN EpochParam ep ON cr.epoch = ep.epochNo "
              + "WHERE (ep.epochNo + ep.committeeMaxTermLength) >= :expiredEpoch")
  Integer countByExpiredEpochNo(@Param("expiredEpoch") Integer expiredEpoch);

  @Query(value =  """
          SELECT COUNT(1) FROM CommitteeRegistration cr 
          WHERE not exists (SELECT 1 FROM CommitteeDeRegistration cd WHERE cd.coldKey = cr.coldKey and cd.blockTime> (
          SELECT MAX(cr2.blockTime) FROM CommitteeRegistration cr2 WHERE cr2.hotKey = cr.hotKey))
          """)
  Long CountByCreatedAtLessThanAndStillActive(@Param("createdAt") Long createdAt);
}
