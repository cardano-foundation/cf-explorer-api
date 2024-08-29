package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

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

  @Query(
      """
    SELECT cr from CommitteeRegistration cr WHERE cr.coldKey IN :coldKeys
    AND NOT EXISTS (SELECT 1 FROM CommitteeRegistration cr2 WHERE cr2.coldKey = cr.coldKey AND cr2.slot > cr.slot)
    """)
  List<CommitteeRegistration> getHotKeyOfCommitteeMemberByColdKeyIn(
      @Param("coldKeys") List<String> coldKeys);

  @Query(
      value =
          "SELECT cr from CommitteeRegistration cr WHERE cr.hotKey = :hotKey ORDER BY cr.slot DESC LIMIT 1")
  CommitteeRegistration getCommitteeRegistrationByHotKey(@Param("hotKey") String hotKey);
}
